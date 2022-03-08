//===-- SimRegisterInfo.cpp - Sim Register Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the Sim implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "SimRegisterInfo.h"
#include "Sim.h"
#include "SimMachineFunctionInfo.h"
#include "SimSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_REGINFO_TARGET_DESC
#include "SimGenRegisterInfo.inc"

static cl::opt<bool>
ReserveAppRegisters("Sim-reserve-app-registers", cl::Hidden, cl::init(false),
                    cl::desc("Reserve application registers (%g2-%g4)"));

SimRegisterInfo::SimRegisterInfo() : SimGenRegisterInfo(SIM::O7) {}

const MCPhysReg*
SimRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_SaveList;
}

const uint32_t *
SimRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                        CallingConv::ID CC) const {
  return CSR_RegMask;
}

const uint32_t*
SimRegisterInfo::getRTCallPreservedMask(CallingConv::ID CC) const {
  return RTCSR_RegMask;
}

BitVector SimRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  const SimSubtarget &Subtarget = MF.getSubtarget<SimSubtarget>();
  // FIXME: G1 reserved for now for large imm generation by frame code.
  Reserved.set(SIM::G1);

  // G1-G4 can be used in applications.
  if (ReserveAppRegisters) {
    Reserved.set(SIM::G2);
    Reserved.set(SIM::G3);
    Reserved.set(SIM::G4);
  }
  Reserved.set(SIM::G5);

  Reserved.set(SIM::R15);
  Reserved.set(SIM::I6);
  Reserved.set(SIM::I7);
  Reserved.set(SIM::G0);
  Reserved.set(SIM::G6);
  Reserved.set(SIM::G7);

  // Also reserve the register pair aliases covering the above
  // registers, with the same conditions.
  Reserved.set(SIM::G0_G1);
  if (ReserveAppRegisters)
    Reserved.set(SIM::G2_G3);
  if (ReserveAppRegisters)
    Reserved.set(SIM::G4_G5);

  Reserved.set(SIM::O6_O7);
  Reserved.set(SIM::I6_I7);
  Reserved.set(SIM::G6_G7);

  // Unaliased double registers are not available in non-V9 targets.
  if (!Subtarget.isV9()) {
    for (unsigned n = 0; n != 16; ++n) {
      for (MCRegAliasIterator AI(SIM::D16 + n, this, true); AI.isValid(); ++AI)
        Reserved.set(*AI);
    }
  }

  // Reserve ASR1-ASR31
  for (unsigned n = 0; n < 31; n++)
    Reserved.set(SIM::ASR1 + n);

  return Reserved;
}

const TargetRegisterClass*
SimRegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                      unsigned Kind) const {
  return &SIM::IntRegsRegClass;
}

static void replaceFI(MachineFunction &MF, MachineBasicBlock::iterator II,
                      MachineInstr &MI, const DebugLoc &dl,
                      unsigned FIOperandNum, int Offset, unsigned FramePtr) {
  // Replace frame index with a frame pointer reference.
  if (Offset >= -4096 && Offset <= 4095) {
    // If the offset is small enough to fit in the immediate field, directly
    // encode it.
    MI.getOperand(FIOperandNum).ChangeToRegister(FramePtr, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
    return;
  }

  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();

  // FIXME: it would be better to scavenge a register here instead of
  // reserving G1 all of the time.
  if (Offset >= 0) {
    // Emit nonnegaive immediates with sethi + or.
    // sethi %hi(Offset), %g1
    // add %g1, %fp, %g1
    // Insert G1+%lo(offset) into the user.
    BuildMI(*MI.getParent(), II, dl, TII.get(SIM::SETHIi), SIM::G1)
      .addImm(HI22(Offset));


    // Emit G1 = G1 + I6
    BuildMI(*MI.getParent(), II, dl, TII.get(SIM::ADDrr), SIM::G1).addReg(SIM::G1)
      .addReg(FramePtr);
    // Insert: G1+%lo(offset) into the user.
    MI.getOperand(FIOperandNum).ChangeToRegister(SIM::G1, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(LO10(Offset));
    return;
  }

  // Emit Negative numbers with sethi + xor
  // sethi %hix(Offset), %g1
  // xor  %g1, %lox(offset), %g1
  // add %g1, %fp, %g1
  // Insert: G1 + 0 into the user.
  BuildMI(*MI.getParent(), II, dl, TII.get(SIM::SETHIi), SIM::G1)
    .addImm(HIX22(Offset));
  BuildMI(*MI.getParent(), II, dl, TII.get(SIM::XORri), SIM::G1)
    .addReg(SIM::G1).addImm(LOX10(Offset));

  BuildMI(*MI.getParent(), II, dl, TII.get(SIM::ADDrr), SIM::G1).addReg(SIM::G1)
    .addReg(FramePtr);
  // Insert: G1+%lo(offset) into the user.
  MI.getOperand(FIOperandNum).ChangeToRegister(SIM::G1, false);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(0);
}


void
SimRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                       int SPAdj, unsigned FIOperandNum,
                                       RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  MachineFunction &MF = *MI.getParent()->getParent();
  const SimSubtarget &Subtarget = MF.getSubtarget<SimSubtarget>();
  const SimFrameLowering *TFI = getFrameLowering(MF);

  Register FrameReg;
  int Offset;
  Offset = TFI->getFrameIndexReference(MF, FrameIndex, FrameReg).getFixed();

  Offset += MI.getOperand(FIOperandNum + 1).getImm();

  if (!Subtarget.hasHardQuad()) {
    if (MI.getOpcode() == SIM::STQFri) {
      const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
      Register SrcReg = MI.getOperand(2).getReg();
      Register SrcEvenReg = getSubReg(SrcReg, SIM::sub_even64);
      Register SrcOddReg = getSubReg(SrcReg, SIM::sub_odd64);
      MachineInstr *StMI =
        BuildMI(*MI.getParent(), II, dl, TII.get(SIM::STDFri))
        .addReg(FrameReg).addImm(0).addReg(SrcEvenReg);
      replaceFI(MF, *StMI, *StMI, dl, 0, Offset, FrameReg);
      MI.setDesc(TII.get(SIM::STDFri));
      MI.getOperand(2).setReg(SrcOddReg);
      Offset += 8;
    } else if (MI.getOpcode() == SIM::LDQFri) {
      const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
      Register DestReg = MI.getOperand(0).getReg();
      Register DestEvenReg = getSubReg(DestReg, SIM::sub_even64);
      Register DestOddReg = getSubReg(DestReg, SIM::sub_odd64);
      MachineInstr *LdMI =
        BuildMI(*MI.getParent(), II, dl, TII.get(SIM::LDDFri), DestEvenReg)
        .addReg(FrameReg).addImm(0);
      replaceFI(MF, *LdMI, *LdMI, dl, 1, Offset, FrameReg);

      MI.setDesc(TII.get(SIM::LDDFri));
      MI.getOperand(0).setReg(DestOddReg);
      Offset += 8;
    }
  }

  replaceFI(MF, II, MI, dl, FIOperandNum, Offset, FrameReg);

}

Register SimRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return SIM::I6;
}

// Sim has no architectural need for stack realignment support,
// except that LLVM unfortunately currently implements overaligned
// stack objects by depending upon stack realignment support.
// If that ever changes, this can probably be deleted.
bool SimRegisterInfo::canRealignStack(const MachineFunction &MF) const {
  if (!TargetRegisterInfo::canRealignStack(MF))
    return false;

  // Sim always has a fixed frame pointer register, so don't need to
  // worry about needing to reserve it. [even if we don't have a frame
  // pointer for our frame, it still cannot be used for other things,
  // or register window traps will be SADNESS.]

  // If there's a reserved call frame, we can use SP to access locals.
  if (getFrameLowering(MF)->hasReservedCallFrame(MF))
    return true;

  // Otherwise, we'd need a base pointer, but those aren't implemented
  // for Sim at the moment.

  return false;
}
