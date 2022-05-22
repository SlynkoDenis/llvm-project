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
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, Align(1), 0, Align(1)),
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
    SimMachineFunctionInfo *FuncInfo = MF.getInfo<SimMachineFunctionInfo>();

    assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");
    MachineFrameInfo &MFI = MF.getFrameInfo();
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
    // if (NumBytes % 4) {
    //   errs() << "WARNING (emitPrologue): NumBytes == " << NumBytes << '\n';
    // }
    NumBytes = NumBytes / 4;
    // Update stack size with corrected value.
    MFI.setStackSize(NumBytes);
    if (NumBytes == 0 && !MFI.adjustsStack()) {
      return;
    }

    // Finally, ensure that the size is sufficiently aligned for the
    // data on the stack.
    // TODO: alignment to 8 bytes, may be needed for long/double args
    // NumBytes = alignTo(NumBytes, MFI.getMaxAlign());

    // Adjust SP for the number of bytes required for this function
    emitRegAdjustment(MBB, MBBI, -NumBytes,
                      MachineInstr::FrameSetup,
                      SIM::SP, SIM::SP);
    
    const auto &CSI = MFI.getCalleeSavedInfo();
    std::advance(MBBI, CSI.size());

    if (!hasFP(MF)) {
      return;
    }

    // FP saved in spillCalleeSavedRegisters

    auto tmp = FuncInfo->getVarArgsSaveSize();
    // TODO: remove this assert
    // if (tmp % 4) {
    //   errs() << "WARNING (emitPrologue): tmp == " << tmp << '\n';
    // }
    NumBytes -= tmp / 4;
    // Set new FP value
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
      // if (Size % 4) {
      //   errs() << "WARNING (eliminateCallFramePseudoInstr): Size == " << Size << '\n';
      // }
      emitRegAdjustment(MBB, I, Size / 4, MachineInstr::NoFlags, SIM::SP, SIM::SP);
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

    auto NumBytes = MFI.getStackSize();
    if (NumBytes == 0 && !MFI.adjustsStack()) {
      return;
    }

    assert(!(MFI.hasVarSizedObjects() && RI->hasStackRealignment(MF)) && "TBD");

    // if (NumBytes % 4) {
    //   errs() << "WARNING (emitEpilogue): NumBytes == " << NumBytes << '\n';
    // }
    // TODO: why can't we restore SP using the saved FP value?
    emitRegAdjustment(MBB, MBBI, NumBytes, MachineInstr::FrameDestroy, SIM::SP, SIM::SP);

    if (!hasFP(MF)) {
      return;
    }
}

bool SimFrameLowering::spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                                 MachineBasicBlock::iterator MI,
                                                 ArrayRef<CalleeSavedInfo> CSI,
                                                 const TargetRegisterInfo *TRI) const {
  if (CSI.empty()) {
    return true;
  }

  const TargetInstrInfo &TII = *ST.getInstrInfo();

  for (auto &CS : CSI) {
    // Insert the spill to the stack frame.
    Register Reg = CS.getReg();
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.storeRegToStackSlot(MBB, MI, Reg, !MBB.isLiveIn(Reg), CS.getFrameIdx(),
                            RC, TRI);
  }

  return true;
}

bool SimFrameLowering::restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                                                   MachineBasicBlock::iterator MI,
                                                   MutableArrayRef<CalleeSavedInfo> CSI,
                                                   const TargetRegisterInfo *TRI) const {
  if (CSI.empty()) {
    return true;
  }

  const TargetInstrInfo &TII = *ST.getInstrInfo();

  // Insert in reverse order.
  // loadRegFromStackSlot can insert multiple instructions.
  for (auto &CS : reverse(CSI)) {
    Register Reg = CS.getReg();
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.loadRegFromStackSlot(MBB, MI, Reg, CS.getFrameIdx(), RC, TRI);
    assert(MI != MBB.begin() && "loadRegFromStackSlot didn't insert any code!");
  }

  return true;
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

static bool isFrameIndexInCalleeSavedRegion(const MachineFrameInfo &MFI,
                                            int FI) {
  // Callee-saved registers should be referenced relative to the stack
  // pointer (positive offset), otherwise use the frame pointer (negative
  // offset).
  const auto &CSI = MFI.getCalleeSavedInfo();
  int MinCSFI = 0;
  int MaxCSFI = -1;
  if (CSI.size()) {
    MinCSFI = CSI[0].getFrameIdx();
    MaxCSFI = CSI[CSI.size() - 1].getFrameIdx();
  }

  return (FI >= MinCSFI && FI <= MaxCSFI);
}

StackOffset
SimFrameLowering::getFrameIndexReference(const MachineFunction &MF, int FI,
                                         Register &FrameReg) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const SimRegisterInfo *RegInfo = ST.getRegisterInfo();
  const SimMachineFunctionInfo *FuncInfo = MF.getInfo<SimMachineFunctionInfo>();
  // TODO: resolve this
  // bool isFixed = MFI.isFixedObjectIndex(FI);

  // TODO: taken from RISCV; need to add VarArgsSaveSize
  int FrameOffset = MFI.getObjectOffset(FI) - getOffsetOfLocalArea() +
                    MFI.getOffsetAdjustment();

  // Addressable stack objects are accessed using neg. offsets from
  // %fp, or positive offsets from %sp.
  bool UseFP;

  if (isFrameIndexInCalleeSavedRegion(MFI, FI)) {
    UseFP = false;
  } else if (FuncInfo->isLeafProc()) {
    // If there's a leaf proc, all offsets need to be %sp-based,
    // because we haven't caused %fp to actually point to our frame.
    // TODO: research this case
    errs() << "FuncInfo->isLeafProc()\n"; 
    UseFP = false;
  } else if (RegInfo->hasStackRealignment(MF)) {
    // TODO: resolve it
    // errs() << "WARNING (getFrameIndexReference): hasStackRealignment -> true\n";
    // llvm_unreachable("TBD");
    // If there is dynamic stack realignment, all local object
    // references need to be via %sp, to take account of the
    // re-alignment.
    // UseFP = false;
    UseFP = true;
  } else {
    // Finally, default to using %fp.
    UseFP = true;
  }

  // if (FrameOffset % 4) {
  //   errs() << "WARNING (emitPrologue): FrameOffset == " << FrameOffset << '\n';
  // }
  FrameOffset = FrameOffset / 4;

  if (UseFP) {
    FrameReg = RegInfo->getFrameRegister(MF);
    return StackOffset::getFixed(FrameOffset);
  } else {
    FrameReg = SIM::SP;
    // errs() << "MF.getFrameInfo().getStackSize() == " << MF.getFrameInfo().getStackSize() << '\n';
    return StackOffset::getFixed(FrameOffset + MF.getFrameInfo().getStackSize());
  }
}

bool SimFrameLowering::hasBP(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo *TRI = ST.getRegisterInfo();

  return MFI.hasVarSizedObjects() && TRI->hasStackRealignment(MF);
}

void SimFrameLowering::remapRegsForLeafProc(MachineFunction &MF) const {
  llvm_unreachable("TBD");
  // MachineRegisterInfo &MRI = MF.getRegInfo();
  // // Remap %i[0-7] to %o[0-7].
  // for (unsigned reg = SP::I0; reg <= SP::I7; ++reg) {
  //   if (!MRI.isPhysRegUsed(reg))
  //     continue;

  //   unsigned mapped_reg = reg - SP::I0 + SP::O0;

  //   // Replace I register with O register.
  //   MRI.replaceRegWith(reg, mapped_reg);

  //   // Also replace register pair super-registers.
  //   if ((reg - SP::I0) % 2 == 0) {
  //     unsigned preg = (reg - SP::I0) / 2 + SP::I0_I1;
  //     unsigned mapped_preg = preg - SP::I0_I1 + SP::O0_O1;
  //     MRI.replaceRegWith(preg, mapped_preg);
  //   }
  // }

  // // Rewrite MBB's Live-ins.
  // for (MachineBasicBlock &MBB : MF) {
  //   for (unsigned reg = SP::I0_I1; reg <= SP::I6_I7; ++reg) {
  //     if (!MBB.isLiveIn(reg))
  //       continue;
  //     MBB.removeLiveIn(reg);
  //     MBB.addLiveIn(reg - SP::I0_I1 + SP::O0_O1);
  //   }
  //   for (unsigned reg = SP::I0; reg <= SP::I7; ++reg) {
  //     if (!MBB.isLiveIn(reg))
  //       continue;
  //     MBB.removeLiveIn(reg);
  //     MBB.addLiveIn(reg - SP::I0 + SP::O0);
  //   }
  // }

  // assert(verifyLeafProcRegUse(&MRI));
}

bool SimFrameLowering::isLeafProc(MachineFunction &MF) const
{
  MachineRegisterInfo &MRI = MF.getRegInfo();
  MachineFrameInfo    &MFI = MF.getFrameInfo();

  // TODO: change R12 to smth else
  return !(MFI.hasCalls()                    // has calls
           || MRI.isPhysRegUsed(SIM::R12)    // Too many registers needed
           || MRI.isPhysRegUsed(SIM::SP)     // %sp is used
           || hasFP(MF));                    // need %fp
}

void SimFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                              BitVector &SavedRegs,
                                              RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);

  if (!DisableLeafProc && isLeafProc(MF)) {
    auto *MFI = MF.getInfo<SimMachineFunctionInfo>();
    MFI->setLeafProc(true);

    remapRegsForLeafProc(MF);
  } else {
    // Unconditionally spill RA and FP only if the function uses a frame
    // pointer.
    if (hasFP(MF)) {
      MachineFrameInfo &MFI = MF.getFrameInfo();
      if (MFI.hasCalls()) {
        SavedRegs.set(SIM::RA);
      }
      SavedRegs.set(SIM::FP);
    } else {
      llvm_unreachable("");
    }
    // Mark BP as used if function has dedicated base pointer.
    if (hasBP(MF)) {
      SavedRegs.set(SIM::BP);
    }
  }
}
