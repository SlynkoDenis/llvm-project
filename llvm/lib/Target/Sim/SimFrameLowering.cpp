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

void SimFrameLowering::emitSPAdjustment(MachineFunction &MF,
                                        MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator MBBI,
                                        int NumBytes) const {

    DebugLoc dl;
    const SimInstrInfo &TII =
      *static_cast<const SimInstrInfo *>(ST.getInstrInfo());

    assert(NumBytes >= -65536 && NumBytes < 65536 && "Invalid SPAdjistment NumBytes argument value");
    BuildMI(MBB, MBBI, dl, TII.get(SIM::ADDi), SIM::SP)
      .addReg(SIM::SP).addImm(NumBytes);
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
  
    // Register FPReg = SIM::FP;
    // Register SPReg = SIM::SP;
  
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
    auto NumBytes = static_cast<int>(MFI.getStackSize());

    if (NumBytes == 0 && !MFI.adjustsStack()) {
      return;
    }

    // unsigned SAVEri = SIM::SAVEri;
    // unsigned SAVErr = SIM::SAVErr;
    // TODO: resolve
    // if (FuncInfo->isLeafProc()) {
    //     if (NumBytes == 0) {
    //         return;
    //     }
    //     SAVEri = SIM::ADDi;
    //     SAVErr = SIM::ADD;
    // }

    // Add the extra call frame stack size, if needed. (This is the same
    // code as in PrologEpilogInserter, but also gets disabled by
    // targetHandlesStackFrameRounding)
    if (MFI.adjustsStack() && hasReservedCallFrame(MF)) {
        NumBytes += MFI.getMaxCallFrameSize();
    }

    // Adds the Sim subtarget-specific spill area to the stack
    // size. Also ensures target-required alignment.
    // NumBytes = ST.getAdjustedFrameSize(NumBytes);

    // Finally, ensure that the size is sufficiently aligned for the
    // data on the stack.
    NumBytes = alignTo(NumBytes, MFI.getMaxAlign());

    // Update stack size with corrected value.
    MFI.setStackSize(NumBytes);

    emitSPAdjustment(MF, MBB, MBBI, -NumBytes);
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
      emitSPAdjustment(MF, MBB, I, Size);
    }
  }
  return MBB.erase(I);
}

void SimFrameLowering::emitEpilogue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
    MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
    DebugLoc dl = MBBI->getDebugLoc();
    assert(MBBI->getOpcode() == SIM::RET &&
           "Can only put epilog before 'ret' instruction!");

    MachineFrameInfo &MFI = MF.getFrameInfo();

    int NumBytes = static_cast<int>(MFI.getStackSize());
    if (NumBytes == 0) {
      return;
    }

    emitSPAdjustment(MF, MBB, MBBI, NumBytes);
}

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

// TODO: resolve
// bool SimFrameLowering::isLeafProc(MachineFunction &MF) const
// {

//   MachineRegisterInfo &MRI = MF.getRegInfo();
//   MachineFrameInfo    &MFI = MF.getFrameInfo();

//   return !(MFI.hasCalls()                  // has calls
//            // || MRI.isPhysRegUsed(SIM::L0)    // Too many registers needed
//            || MRI.isPhysRegUsed(SIM::SP)    // %sp is used
//            || hasFP(MF));                  // need %fp
// }

// TODO: resolve
// void SimFrameLowering::remapRegsForLeafProc(MachineFunction &MF) const {
//   MachineRegisterInfo &MRI = MF.getRegInfo();
//   // Remap %i[0-7] to %o[0-7].
//   for (unsigned reg = SIM::I0; reg <= SIM::I7; ++reg) {
//     if (!MRI.isPhysRegUsed(reg))
//       continue;

//     unsigned mapped_reg = reg - SIM::I0 + SIM::O0;

//     // Replace I register with O register.
//     MRI.replaceRegWith(reg, mapped_reg);

//     // Also replace register pair super-registers.
//     if ((reg - SIM::I0) % 2 == 0) {
//       unsigned preg = (reg - SIM::I0) / 2 + SIM::I0_I1;
//       unsigned mapped_preg = preg - SIM::I0_I1 + SIM::O0_O1;
//       MRI.replaceRegWith(preg, mapped_preg);
//     }
//   }

//   // Rewrite MBB's Live-ins.
//   for (MachineBasicBlock &MBB : MF) {
//     for (unsigned reg = SIM::I0_I1; reg <= SIM::I6_I7; ++reg) {
//       if (!MBB.isLiveIn(reg))
//         continue;
//       MBB.removeLiveIn(reg);
//       MBB.addLiveIn(reg - SIM::I0_I1 + SIM::O0_O1);
//     }
//     for (unsigned reg = SIM::I0; reg <= SIM::I7; ++reg) {
//       if (!MBB.isLiveIn(reg))
//         continue;
//       MBB.removeLiveIn(reg);
//       MBB.addLiveIn(reg - SIM::I0 + SIM::O0);
//     }
//   }

//     assert(verifyLeafProcRegUse(&MRI));
// #ifdef EXPENSIVE_CHECKS
//     MF.verify(0, "After LeafProc Remapping");
// #endif
// }

void SimFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                              BitVector &SavedRegs,
                                              RegScavenger *RS) const {
    TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
    if (hasFP(MF)) {
      SavedRegs.set(SIM::SP);
      // SavedRegs.set(SIM::FP);
    }
    // TODO: resolve
    // if (!DisableLeafProc && isLeafProc(MF)) {
    //     SimMachineFunctionInfo *MFI = MF.getInfo<SimMachineFunctionInfo>();
    //     MFI->setLeafProc(true);

    //     remapRegsForLeafProc(MF);
    // }
}
