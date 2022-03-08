//===-- SimInstrInfo.cpp - Sim Instruction Information ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the Sim implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "SimInstrInfo.h"
#include "Sim.h"
#include "SimMachineFunctionInfo.h"
#include "SimSubtarget.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "SimGenInstrInfo.inc"

// Pin the vtable to this file.
void SimInstrInfo::anchor() {}

SimInstrInfo::SimInstrInfo(SimSubtarget &ST)
    : SimGenInstrInfo(SIM::ADJCALLSTACKDOWN, SIM::ADJCALLSTACKUP), RI(),
      Subtarget(ST) {}

/// isLoadFromStackSlot - If the specified machine instruction is a direct
/// load from a stack slot, return the virtual or physical register number of
/// the destination along with the FrameIndex of the loaded stack slot. If
/// not, return 0. This predicate must return 0 if the instruction has
/// any side effects other than loading from the stack slot.
unsigned SimInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                             int &FrameIndex) const {
  // TODO: add SIM::LD here?
  if (MI.getOpcode() == SIM::LDi) {
    if (MI.getOperand(1).isFI() && MI.getOperand(2).isImm() &&
        MI.getOperand(2).getImm() == 0) {
      FrameIndex = MI.getOperand(1).getIndex();
      return MI.getOperand(0).getReg();
    }
  }
  return 0;
}

/// isStoreToStackSlot - If the specified machine instruction is a direct
/// store to a stack slot, return the virtual or physical register number of
/// the source reg along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than storing to the stack slot.
unsigned SimInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                            int &FrameIndex) const {
  if (MI.getOpcode() == SIM::STi) {
    if (MI.getOperand(0).isFI() && MI.getOperand(1).isImm() &&
        MI.getOperand(1).getImm() == 0) {
      FrameIndex = MI.getOperand(0).getIndex();
      return MI.getOperand(2).getReg();
    }
  }
  return 0;
}

static bool IsIntegerCC(unsigned CC)
{
  return  (CC <= SPCC::ICC_VC);
}

static SPCC::CondCodes GetOppositeBranchCondition(SPCC::CondCodes CC)
{
  switch(CC) {
  case SPCC::ICC_A:    return SPCC::ICC_N;
  case SPCC::ICC_N:    return SPCC::ICC_A;
  case SPCC::ICC_NE:   return SPCC::ICC_E;
  case SPCC::ICC_E:    return SPCC::ICC_NE;
  case SPCC::ICC_G:    return SPCC::ICC_LE;
  case SPCC::ICC_LE:   return SPCC::ICC_G;
  case SPCC::ICC_GE:   return SPCC::ICC_L;
  case SPCC::ICC_L:    return SPCC::ICC_GE;
  case SPCC::ICC_GU:   return SPCC::ICC_LEU;
  case SPCC::ICC_LEU:  return SPCC::ICC_GU;
  case SPCC::ICC_CC:   return SPCC::ICC_CS;
  case SPCC::ICC_CS:   return SPCC::ICC_CC;
  case SPCC::ICC_POS:  return SPCC::ICC_NEG;
  case SPCC::ICC_NEG:  return SPCC::ICC_POS;
  case SPCC::ICC_VC:   return SPCC::ICC_VS;
  case SPCC::ICC_VS:   return SPCC::ICC_VC;

  case SPCC::FCC_A:    return SPCC::FCC_N;
  case SPCC::FCC_N:    return SPCC::FCC_A;
  case SPCC::FCC_U:    return SPCC::FCC_O;
  case SPCC::FCC_O:    return SPCC::FCC_U;
  case SPCC::FCC_G:    return SPCC::FCC_ULE;
  case SPCC::FCC_LE:   return SPCC::FCC_UG;
  case SPCC::FCC_UG:   return SPCC::FCC_LE;
  case SPCC::FCC_ULE:  return SPCC::FCC_G;
  case SPCC::FCC_L:    return SPCC::FCC_UGE;
  case SPCC::FCC_GE:   return SPCC::FCC_UL;
  case SPCC::FCC_UL:   return SPCC::FCC_GE;
  case SPCC::FCC_UGE:  return SPCC::FCC_L;
  case SPCC::FCC_LG:   return SPCC::FCC_UE;
  case SPCC::FCC_UE:   return SPCC::FCC_LG;
  case SPCC::FCC_NE:   return SPCC::FCC_E;
  case SPCC::FCC_E:    return SPCC::FCC_NE;

  case SPCC::CPCC_A:   return SPCC::CPCC_N;
  case SPCC::CPCC_N:   return SPCC::CPCC_A;
  case SPCC::CPCC_3:   LLVM_FALLTHROUGH;
  case SPCC::CPCC_2:   LLVM_FALLTHROUGH;
  case SPCC::CPCC_23:  LLVM_FALLTHROUGH;
  case SPCC::CPCC_1:   LLVM_FALLTHROUGH;
  case SPCC::CPCC_13:  LLVM_FALLTHROUGH;
  case SPCC::CPCC_12:  LLVM_FALLTHROUGH;
  case SPCC::CPCC_123: LLVM_FALLTHROUGH;
  case SPCC::CPCC_0:   LLVM_FALLTHROUGH;
  case SPCC::CPCC_03:  LLVM_FALLTHROUGH;
  case SPCC::CPCC_02:  LLVM_FALLTHROUGH;
  case SPCC::CPCC_023: LLVM_FALLTHROUGH;
  case SPCC::CPCC_01:  LLVM_FALLTHROUGH;
  case SPCC::CPCC_013: LLVM_FALLTHROUGH;
  case SPCC::CPCC_012:
      // "Opposite" code is not meaningful, as we don't know
      // what the CoProc condition means here. The cond-code will
      // only be used in inline assembler, so this code should
      // not be reached in a normal compilation pass.
      llvm_unreachable("Meaningless inversion of co-processor cond code");
  }
  llvm_unreachable("Invalid cond code");
}

static bool isUncondBranchOpcode(int Opc) { return Opc == SIM::BR; }

static bool isCondBranchOpcode(int Opc) {
  return Opc == SIM::BEQ || Opc == SIM::BNE || Opc == SIM::BGT || Opc == SIM::BLE;
}

static bool isIndirectBranchOpcode(int Opc) {
  return Opc == SIM::B || Opc == SIM::BR;
}

static void parseCondBranch(MachineInstr *LastInst, MachineBasicBlock *&Target,
                            SmallVectorImpl<MachineOperand> &Cond) {
  Cond.push_back(MachineOperand::CreateImm(LastInst->getOperand(1).getImm()));
  Target = LastInst->getOperand(0).getMBB();
}

bool SimInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                   MachineBasicBlock *&TBB,
                                   MachineBasicBlock *&FBB,
                                   SmallVectorImpl<MachineOperand> &Cond,
                                   bool AllowModify) const {
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return false;

  if (!isUnpredicatedTerminator(*I))
    return false;

  // Get the last instruction in the block.
  MachineInstr *LastInst = &*I;
  unsigned LastOpc = LastInst->getOpcode();

  // If there is only one terminator instruction, process it.
  if (I == MBB.begin() || !isUnpredicatedTerminator(*--I)) {
    if (isUncondBranchOpcode(LastOpc)) {
      TBB = LastInst->getOperand(0).getMBB();
      return false;
    }
    if (isCondBranchOpcode(LastOpc)) {
      // Block ends with fall-through condbranch.
      parseCondBranch(LastInst, TBB, Cond);
      return false;
    }
    return true; // Can't handle indirect branch.
  }

  // Get the instruction before it if it is a terminator.
  MachineInstr *SecondLastInst = &*I;
  unsigned SecondLastOpc = SecondLastInst->getOpcode();

  // If AllowModify is true and the block ends with two or more unconditional
  // branches, delete all but the first unconditional branch.
  if (AllowModify && isUncondBranchOpcode(LastOpc)) {
    while (isUncondBranchOpcode(SecondLastOpc)) {
      LastInst->eraseFromParent();
      LastInst = SecondLastInst;
      LastOpc = LastInst->getOpcode();
      if (I == MBB.begin() || !isUnpredicatedTerminator(*--I)) {
        // Return now the only terminator is an unconditional branch.
        TBB = LastInst->getOperand(0).getMBB();
        return false;
      } else {
        SecondLastInst = &*I;
        SecondLastOpc = SecondLastInst->getOpcode();
      }
    }
  }

  // If there are three terminators, we don't know what sort of block this is.
  if (SecondLastInst && I != MBB.begin() && isUnpredicatedTerminator(*--I))
    return true;

  // If the block ends with a B and a Bcc, handle it.
  if (isCondBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
    parseCondBranch(SecondLastInst, TBB, Cond);
    FBB = LastInst->getOperand(0).getMBB();
    return false;
  }

  // If the block ends with two unconditional branches, handle it.  The second
  // one is not executed.
  if (isUncondBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
    TBB = SecondLastInst->getOperand(0).getMBB();
    return false;
  }

  // ...likewise if it ends with an indirect branch followed by an unconditional
  // branch.
  if (isIndirectBranchOpcode(SecondLastOpc) && isUncondBranchOpcode(LastOpc)) {
    I = LastInst;
    if (AllowModify)
      I->eraseFromParent();
    return true;
  }

  // Otherwise, can't handle this.
  return true;
}

unsigned SimInstrInfo::insertBranch(MachineBasicBlock &MBB,
                                      MachineBasicBlock *TBB,
                                      MachineBasicBlock *FBB,
                                      ArrayRef<MachineOperand> Cond,
                                      const DebugLoc &DL,
                                      int *BytesAdded) const {
  assert(TBB && "insertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 1 || Cond.size() == 0) &&
         "Sim branch conditions should have one component!");
  assert(!BytesAdded && "code size not handled");

  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(SIM::BA)).addMBB(TBB);
    return 1;
  }

  // Conditional branch
  unsigned CC = Cond[0].getImm();

  if (IsIntegerCC(CC))
    BuildMI(&MBB, DL, get(SIM::BCOND)).addMBB(TBB).addImm(CC);
  else
    BuildMI(&MBB, DL, get(SIM::FBCOND)).addMBB(TBB).addImm(CC);
  if (!FBB)
    return 1;

  BuildMI(&MBB, DL, get(SIM::BA)).addMBB(FBB);
  return 2;
}

unsigned SimInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                      int *BytesRemoved) const {
  assert(!BytesRemoved && "code size not handled");

  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugInstr())
      continue;

    if (I->getOpcode() != SIM::BA
        && I->getOpcode() != SIM::BCOND
        && I->getOpcode() != SIM::FBCOND)
      break; // Not a branch

    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
}

bool SimInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() == 1);
  SPCC::CondCodes CC = static_cast<SPCC::CondCodes>(Cond[0].getImm());
  Cond[0].setImm(GetOppositeBranchCondition(CC));
  return false;
}

void SimInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator I,
                                 const DebugLoc &DL, MCRegister DestReg,
                                 MCRegister SrcReg, bool KillSrc) const {
  unsigned numSubRegs = 0;
  unsigned movOpc     = 0;
  const unsigned *subRegIdx = nullptr;
  bool ExtraG0 = false;

  const unsigned DW_SubRegsIdx[]  = { SIM::sub_even, SIM::sub_odd };
  const unsigned DFP_FP_SubRegsIdx[]  = { SIM::sub_even, SIM::sub_odd };
  const unsigned QFP_DFP_SubRegsIdx[] = { SIM::sub_even64, SIM::sub_odd64 };
  const unsigned QFP_FP_SubRegsIdx[]  = { SIM::sub_even, SIM::sub_odd,
                                          SIM::sub_odd64_then_sub_even,
                                          SIM::sub_odd64_then_sub_odd };

  if (SIM::IntRegsRegClass.contains(DestReg, SrcReg))
    BuildMI(MBB, I, DL, get(SIM::ORrr), DestReg).addReg(SIM::G0)
      .addReg(SrcReg, getKillRegState(KillSrc));
  else if (SIM::IntPairRegClass.contains(DestReg, SrcReg)) {
    subRegIdx  = DW_SubRegsIdx;
    numSubRegs = 2;
    movOpc     = SIM::ORrr;
    ExtraG0 = true;
  } else if (SIM::FPRegsRegClass.contains(DestReg, SrcReg))
    BuildMI(MBB, I, DL, get(SIM::FMOVS), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
  else if (SIM::DFPRegsRegClass.contains(DestReg, SrcReg)) {
    if (Subtarget.isV9()) {
      BuildMI(MBB, I, DL, get(SIM::FMOVD), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
    } else {
      // Use two FMOVS instructions.
      subRegIdx  = DFP_FP_SubRegsIdx;
      numSubRegs = 2;
      movOpc     = SIM::FMOVS;
    }
  } else if (SIM::QFPRegsRegClass.contains(DestReg, SrcReg)) {
    if (Subtarget.isV9()) {
      if (Subtarget.hasHardQuad()) {
        BuildMI(MBB, I, DL, get(SIM::FMOVQ), DestReg)
          .addReg(SrcReg, getKillRegState(KillSrc));
      } else {
        // Use two FMOVD instructions.
        subRegIdx  = QFP_DFP_SubRegsIdx;
        numSubRegs = 2;
        movOpc     = SIM::FMOVD;
      }
    } else {
      // Use four FMOVS instructions.
      subRegIdx  = QFP_FP_SubRegsIdx;
      numSubRegs = 4;
      movOpc     = SIM::FMOVS;
    }
  } else if (SIM::ASRRegsRegClass.contains(DestReg) &&
             SIM::IntRegsRegClass.contains(SrcReg)) {
    BuildMI(MBB, I, DL, get(SIM::WRASRrr), DestReg)
        .addReg(SIM::G0)
        .addReg(SrcReg, getKillRegState(KillSrc));
  } else if (SIM::IntRegsRegClass.contains(DestReg) &&
             SIM::ASRRegsRegClass.contains(SrcReg)) {
    BuildMI(MBB, I, DL, get(SIM::RDASR), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
  } else
    llvm_unreachable("Impossible reg-to-reg copy");

  if (numSubRegs == 0 || subRegIdx == nullptr || movOpc == 0)
    return;

  const TargetRegisterInfo *TRI = &getRegisterInfo();
  MachineInstr *MovMI = nullptr;

  for (unsigned i = 0; i != numSubRegs; ++i) {
    Register Dst = TRI->getSubReg(DestReg, subRegIdx[i]);
    Register Src = TRI->getSubReg(SrcReg, subRegIdx[i]);
    assert(Dst && Src && "Bad sub-register");

    MachineInstrBuilder MIB = BuildMI(MBB, I, DL, get(movOpc), Dst);
    if (ExtraG0)
      MIB.addReg(SIM::G0);
    MIB.addReg(Src);
    MovMI = MIB.getInstr();
  }
  // Add implicit super-register defs and kills to the last MovMI.
  MovMI->addRegisterDefined(DestReg, TRI);
  if (KillSrc)
    MovMI->addRegisterKilled(SrcReg, TRI);
}

void SimInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                    Register SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  const MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOStore,
      MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  // On the order of operands here: think "[FrameIdx + 0] = SrcReg".
  if (RC == &SIM::I64RegsRegClass)
    BuildMI(MBB, I, DL, get(SIM::STXri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill)).addMemOperand(MMO);
  else if (RC == &SIM::IntRegsRegClass)
    BuildMI(MBB, I, DL, get(SIM::STri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill)).addMemOperand(MMO);
  else if (RC == &SIM::IntPairRegClass)
    BuildMI(MBB, I, DL, get(SIM::STDri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill)).addMemOperand(MMO);
  else if (RC == &SIM::FPRegsRegClass)
    BuildMI(MBB, I, DL, get(SIM::STFri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg,  getKillRegState(isKill)).addMemOperand(MMO);
  else if (SIM::DFPRegsRegClass.hasSubClassEq(RC))
    BuildMI(MBB, I, DL, get(SIM::STDFri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg,  getKillRegState(isKill)).addMemOperand(MMO);
  else if (SIM::QFPRegsRegClass.hasSubClassEq(RC))
    // Use STQFri irrespective of its legality. If STQ is not legal, it will be
    // lowered into two STDs in eliminateFrameIndex.
    BuildMI(MBB, I, DL, get(SIM::STQFri)).addFrameIndex(FI).addImm(0)
      .addReg(SrcReg,  getKillRegState(isKill)).addMemOperand(MMO);
  else
    llvm_unreachable("Can't store this register to stack slot");
}

void SimInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                     Register DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  const MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOLoad,
      MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  if (RC == &SIM::I64RegsRegClass)
    BuildMI(MBB, I, DL, get(SIM::LDXri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (RC == &SIM::IntRegsRegClass)
    BuildMI(MBB, I, DL, get(SIM::LDri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (RC == &SIM::IntPairRegClass)
    BuildMI(MBB, I, DL, get(SIM::LDDri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (RC == &SIM::FPRegsRegClass)
    BuildMI(MBB, I, DL, get(SIM::LDFri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (SIM::DFPRegsRegClass.hasSubClassEq(RC))
    BuildMI(MBB, I, DL, get(SIM::LDDFri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else if (SIM::QFPRegsRegClass.hasSubClassEq(RC))
    // Use LDQFri irrespective of its legality. If LDQ is not legal, it will be
    // lowered into two LDDs in eliminateFrameIndex.
    BuildMI(MBB, I, DL, get(SIM::LDQFri), DestReg).addFrameIndex(FI).addImm(0)
      .addMemOperand(MMO);
  else
    llvm_unreachable("Can't load this register from stack slot");
}

Register SimInstrInfo::getGlobalBaseReg(MachineFunction *MF) const {
  SimMachineFunctionInfo *SimFI = MF->getInfo<SimMachineFunctionInfo>();
  Register GlobalBaseReg = SimFI->getGlobalBaseReg();
  if (GlobalBaseReg)
    return GlobalBaseReg;

  // Insert the set of GlobalBaseReg into the first MBB of the function
  MachineBasicBlock &FirstMBB = MF->front();
  MachineBasicBlock::iterator MBBI = FirstMBB.begin();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();

  const TargetRegisterClass *PtrRC =
    Subtarget.is64Bit() ? &SIM::I64RegsRegClass : &SIM::IntRegsRegClass;
  GlobalBaseReg = RegInfo.createVirtualRegister(PtrRC);

  DebugLoc dl;

  BuildMI(FirstMBB, MBBI, dl, get(SIM::GETPCX), GlobalBaseReg);
  SimFI->setGlobalBaseReg(GlobalBaseReg);
  return GlobalBaseReg;
}

bool SimInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  case TargetOpcode::LOAD_STACK_GUARD: {
    assert(Subtarget.isTargetLinux() &&
           "Only Linux target is expected to contain LOAD_STACK_GUARD");
    // offsetof(tcbhead_t, stack_guard) from sysdeps/Sim/nptl/tls.h in glibc.
    const int64_t Offset = Subtarget.is64Bit() ? 0x28 : 0x14;
    MI.setDesc(get(Subtarget.is64Bit() ? SIM::LDXri : SIM::LDri));
    MachineInstrBuilder(*MI.getParent()->getParent(), MI)
        .addReg(SIM::G7)
        .addImm(Offset);
    return true;
  }
  }
  return false;
}
