//===-- SimFrameLowering.cpp - Sim Frame Information ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the Sim implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "Sim.h"
#include "SimFrameLowering.h"
#include "SimInstrInfo.h"
#include "SimMachineFunctionInfo.h"
#include "SimSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

static cl::opt<bool>
DisableLeafProc("disable-Sim-leaf-proc",
                cl::init(false),
                cl::desc("Disable Sim leaf procedure optimization."),
                cl::Hidden);

SimFrameLowering::SimFrameLowering(const SimSubtarget &ST)
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, Align(4), 0, Align(4)),
      ST(ST) {}

void SimFrameLowering::emitRegAdjustment(MachineBasicBlock &MBB,
                                         MachineBasicBlock::iterator MBBI,
                                         int NumBytes,
                                         MachineInstr::MIFlag Flag,
                                         Register Src, Register Dest) const {

    DebugLoc dl;
    const SimInstrInfo &TII =
      *static_cast<const SimInstrInfo *>(ST.getInstrInfo());

    assert(isInt<16>(NumBytes) && "Invalid SPAdjistment NumBytes argument value");
    BuildMI(MBB, MBBI, dl, TII.get(SIM::ADDi), Dest)
      .addReg(Src).addImm(NumBytes).setMIFlag(Flag);
    return;
}

void SimFrameLowering::emitPrologue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
    // SimMachineFunctionInfo *FuncInfo = MF.getInfo<SimMachineFunctionInfo>();

    assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");
    MachineFrameInfo &MFI = MF.getFrameInfo();
    // const SimInstrInfo &TII =
    //     *static_cast<const SimInstrInfo *>(ST.getInstrInfo());
    const SimRegisterInfo &RegInfo =
        *static_cast<const SimRegisterInfo *>(ST.getRegisterInfo());
    MachineBasicBlock::iterator MBBI = MBB.begin();
  
    // Debug location must be unknown since the first debug location is used
    // to determine the end of the prologue.
    DebugLoc dl;
    bool NeedsStackRealignment = RegInfo.shouldRealignStack(MF);
    if (NeedsStackRealignment && !RegInfo.canRealignStack(MF)) {
      report_fatal_error("Function \"" + Twine(MF.getName()) + "\" required "
                         "stack re-alignment, but LLVM couldn't handle it "
                         "(probably because it has a dynamic alloca).");
    }

    // Get the number of bytes to allocate from the FrameInfo
    auto NumBytes = alignTo(MFI.getStackSize(), getStackAlign());
    if (NumBytes == 0 && !MFI.adjustsStack()) {
      return;
    }

    // Adds the Sim subtarget-specific spill area to the stack
    // size. Also ensures target-required alignment.
    // NumBytes = ST.getAdjustedFrameSize(NumBytes);

    // Finally, ensure that the size is sufficiently aligned for the
    // data on the stack.
    NumBytes = alignTo(NumBytes, MFI.getMaxAlign());

    // Update stack size with corrected value.
    MFI.setStackSize(NumBytes);

    emitRegAdjustment(MBB, MBBI, -NumBytes,
                      MachineInstr::FrameSetup,
                      SIM::SP, SIM::SP);

    if (!hasFP(MF)) {
      return;
    }
    
    const auto &CSI = MFI.getCalleeSavedInfo();
    std::advance(MBBI, CSI.size());

    emitRegAdjustment(MBB, MBBI, NumBytes,
                      MachineInstr::FrameSetup,
                      SIM::SP, SIM::FP);
}

MachineBasicBlock::iterator SimFrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  if (!hasReservedCallFrame(MF)) {
    MachineInstr &MI = *I;
    int Size = MI.getOperand(0).getImm();
    if (MI.getOpcode() == SIM::ADJCALLSTACKDOWN) {
      Size = -Size;
    }

    if (Size) {
      emitRegAdjustment(MBB, I, Size, MachineInstr::NoFlags, SIM::SP, SIM::SP);
    }
  }
  return MBB.erase(I);
}

void SimFrameLowering::emitEpilogue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
    const SimRegisterInfo *RI = ST.getRegisterInfo();
    MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
    DebugLoc dl = MBBI->getDebugLoc();
    // auto *FI = MF.getInfo<SimMachineFunctionInfo>();
    // assert(FI->getVarArgsSaveSize() == 0 && "getVarArgsSaveSize must be zero!");
    // assert(MBBI->getOpcode() == SIM::RET &&
    //        "Can only put epilog before 'ret' instruction!");
    MachineFrameInfo &MFI = MF.getFrameInfo();

    auto NumBytes = alignTo(MFI.getStackSize(), getStackAlign());
    if (NumBytes == 0 && !MFI.adjustsStack()) {
      return;
    }

    assert(hasFP(MF) && "must have FP!");
    assert(!(MFI.hasVarSizedObjects() && RI->hasStackRealignment(MF)) && "TBD");

    emitRegAdjustment(MBB, MBBI, NumBytes, MachineInstr::FrameDestroy, SIM::SP, SIM::SP);
}

// TODO: delete this function and usage
bool SimFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
    // Reserve call frame if there are no variable sized objects on the stack.
    return !MF.getFrameInfo().hasVarSizedObjects();
}

// hasFP - Return true if the specified function should have a dedicated frame
// pointer register.  This is true if the function has variable sized allocas or
// if frame pointer elimination is disabled.
bool SimFrameLowering::hasFP(const MachineFunction &MF) const {
  const TargetRegisterInfo *RegInfo = ST.getRegisterInfo();

  const MachineFrameInfo &MFI = MF.getFrameInfo();
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         RegInfo->hasStackRealignment(MF) || MFI.hasVarSizedObjects() ||
         MFI.isFrameAddressTaken();
}

StackOffset
SimFrameLowering::getFrameIndexReference(const MachineFunction &MF, int FI,
                                           Register &FrameReg) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const SimRegisterInfo *RegInfo = ST.getRegisterInfo();
  // const SimMachineFunctionInfo *FuncInfo = MF.getInfo<SimMachineFunctionInfo>();
  bool isFixed = MFI.isFixedObjectIndex(FI);

  // Addressable stack objects are accessed using neg. offsets from
  // %fp, or positive offsets from %sp.
  bool UseFP;

  // TODO: resolve
  // Sim uses FP-based references in general, even when "hasFP" is
  // false. That function is rather a misnomer, because %fp is
  // actually always available, unless isLeafProc.
  /*if (FuncInfo->isLeafProc()) {
    // If there's a leaf proc, all offsets need to be %sp-based,
    // because we haven't caused %fp to actually point to our frame.
    UseFP = false;
  } else*/ if (isFixed) {
    // Otherwise, argument access should always use %fp.
    UseFP = true;
  } else if (RegInfo->hasStackRealignment(MF)) {
    // If there is dynamic stack realignment, all local object
    // references need to be via %sp, to take account of the
    // re-alignment.
    UseFP = false;
  } else {
    // Finally, default to using %fp.
    UseFP = true;
  }

  int64_t FrameOffset = MF.getFrameInfo().getObjectOffset(FI) +
      ST.getStackPointerBias();

  if (UseFP) {
    FrameReg = RegInfo->getFrameRegister(MF);
    return StackOffset::getFixed(FrameOffset);
  } else {
    FrameReg = SIM::SP; // %sp
    return StackOffset::getFixed(FrameOffset + MF.getFrameInfo().getStackSize());
  }
}

void SimFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                              BitVector &SavedRegs,
                                              RegScavenger *RS) const {
    TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
    if (hasFP(MF)) {
      SavedRegs.set(SIM::SP);
      // SavedRegs.set(SIM::FP);
    }
}
