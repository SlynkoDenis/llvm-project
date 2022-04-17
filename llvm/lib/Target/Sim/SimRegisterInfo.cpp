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

SimRegisterInfo::SimRegisterInfo() : SimGenRegisterInfo(SIM::R1) {}

const MCPhysReg*
SimRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_Sim_SaveList;
}

const uint32_t *
SimRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                        CallingConv::ID CC) const {
  return CSR_Sim_RegMask;
}

const uint32_t*
SimRegisterInfo::getRTCallPreservedMask(CallingConv::ID CC) const {
  return CSR_Sim_RegMask;
}

BitVector SimRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());

  Reserved.set(SIM::R10);
  Reserved.set(SIM::R11);
  Reserved.set(SIM::R12);
  Reserved.set(SIM::R13);
  Reserved.set(SIM::R14);

  return Reserved;
}

Register SimRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = getFrameLowering(MF);
  return TFI->hasFP(MF) ? SIM::FP : SIM::SP;
}

// const TargetRegisterClass*
// SimRegisterInfo::getPointerRegClass(const MachineFunction &MF,
//                                       unsigned Kind) const {
//   return &SIM::IntRegsRegClass;
// }

// static void replaceFI(MachineFunction &MF, MachineBasicBlock::iterator II,
//                       MachineInstr &MI, const DebugLoc &dl,
//                       unsigned FIOperandNum, int Offset, unsigned FramePtr) {
//   // Replace frame index with a frame pointer reference.
//   if (Offset >= -4096 && Offset <= 4095) {
//     // If the offset is small enough to fit in the immediate field, directly
//     // encode it.
//     MI.getOperand(FIOperandNum).ChangeToRegister(FramePtr, false);
//     MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
//     return;
//   }

//   const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();

//   // FIXME: it would be better to scavenge a register here instead of
//   // reserving G1 all of the time.
//   if (Offset >= 0) {
//     // Emit nonnegaive immediates with sethi + or.
//     // sethi %hi(Offset), %g1
//     // add %g1, %fp, %g1
//     // Insert G1+%lo(offset) into the user.
//     BuildMI(*MI.getParent(), II, dl, TII.get(SIM::SETHIi), SIM::G1)
//       .addImm(HI22(Offset));


//     // Emit G1 = G1 + I6
//     BuildMI(*MI.getParent(), II, dl, TII.get(SIM::ADDrr), SIM::G1).addReg(SIM::G1)
//       .addReg(FramePtr);
//     // Insert: G1+%lo(offset) into the user.
//     MI.getOperand(FIOperandNum).ChangeToRegister(SIM::G1, false);
//     MI.getOperand(FIOperandNum + 1).ChangeToImmediate(LO10(Offset));
//     return;
//   }

//   // Emit Negative numbers with sethi + xor
//   // sethi %hix(Offset), %g1
//   // xor  %g1, %lox(offset), %g1
//   // add %g1, %fp, %g1
//   // Insert: G1 + 0 into the user.
//   BuildMI(*MI.getParent(), II, dl, TII.get(SIM::SETHIi), SIM::G1)
//     .addImm(HIX22(Offset));
//   BuildMI(*MI.getParent(), II, dl, TII.get(SIM::XORri), SIM::G1)
//     .addReg(SIM::G1).addImm(LOX10(Offset));

//   BuildMI(*MI.getParent(), II, dl, TII.get(SIM::ADDrr), SIM::G1).addReg(SIM::G1)
//     .addReg(FramePtr);
//   // Insert: G1+%lo(offset) into the user.
//   MI.getOperand(FIOperandNum).ChangeToRegister(SIM::G1, false);
//   MI.getOperand(FIOperandNum + 1).ChangeToImmediate(0);
// }

void
SimRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                       int SPAdj, unsigned FIOperandNum,
                                       RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  MachineFunction &MF = *MI.getParent()->getParent();
  const SimFrameLowering *TFI = getFrameLowering(MF);

  Register FrameReg;
  int Offset;
  Offset = TFI->getFrameIndexReference(MF, FrameIndex, FrameReg).getFixed();

  Offset += MI.getOperand(FIOperandNum + 1).getImm();

  if (!isInt<16>(Offset)) {
    llvm_unreachable("");
  }

    // if (MI.getOpcode() == SIM::STQFri) {
    //   const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
    //   Register SrcReg = MI.getOperand(2).getReg();
    //   Register SrcEvenReg = getSubReg(SrcReg, SIM::sub_even64);
    //   Register SrcOddReg = getSubReg(SrcReg, SIM::sub_odd64);
    //   MachineInstr *StMI =
    //     BuildMI(*MI.getParent(), II, dl, TII.get(SIM::STDFri))
    //     .addReg(FrameReg).addImm(0).addReg(SrcEvenReg);
    //   replaceFI(MF, *StMI, *StMI, dl, 0, Offset, FrameReg);
    //   MI.setDesc(TII.get(SIM::STDFri));
    //   MI.getOperand(2).setReg(SrcOddReg);
    //   Offset += 8;
    // } else if (MI.getOpcode() == SIM::LDQFri) {
    //   const TargetInstrInfo &TII = *Subtarget.getInstrInfo();
    //   Register DestReg = MI.getOperand(0).getReg();
    //   Register DestEvenReg = getSubReg(DestReg, SIM::sub_even64);
    //   Register DestOddReg = getSubReg(DestReg, SIM::sub_odd64);
    //   MachineInstr *LdMI =
    //     BuildMI(*MI.getParent(), II, dl, TII.get(SIM::LDDFri), DestEvenReg)
    //     .addReg(FrameReg).addImm(0);
    //   replaceFI(MF, *LdMI, *LdMI, dl, 1, Offset, FrameReg);

    //   MI.setDesc(TII.get(SIM::LDDFri));
    //   MI.getOperand(0).setReg(DestOddReg);
    //   Offset += 8;
    // }

  MI.getOperand(FIOperandNum).ChangeToRegister(FrameReg, false, false, false);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
}
