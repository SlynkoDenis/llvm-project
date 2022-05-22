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
#include "MCTargetDesc/SimInfo.h"
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
    if (MI.getOperand(1).isFI() && MI.getOperand(2).isImm() &&
        MI.getOperand(2).getImm() == 0) {
      FrameIndex = MI.getOperand(1).getIndex();
      return MI.getOperand(0  ).getReg();
    }
  }
  return 0;
}

bool SimInstrInfo::verifyInstruction(const MachineInstr &MI,
                                     StringRef &ErrInfo) const {
  const MCInstrInfo *MCII = Subtarget.getInstrInfo();
  MCInstrDesc const &Desc = MCII->get(MI.getOpcode());

  for (auto &OI : enumerate(Desc.operands())) {
    unsigned OpType = OI.value().OperandType;
    if (OpType >= SimOp::OPERAND_SIMM16 &&
        OpType <= SimOp::OPERAND_UIMM5) {
      const MachineOperand &MO = MI.getOperand(OI.index());
      if (MO.isImm()) {
        int64_t Imm = MO.getImm();
        bool Ok;
        switch (OpType) {
        default:
          llvm_unreachable("Unexpected operand type");
        case SimOp::OPERAND_SIMM16:
          Ok = isInt<16>(Imm);
          break;
        case SimOp::OPERAND_UIMM16:
          Ok = isUInt<16>(Imm);
          break;
        case SimOp::OPERAND_UIMM5:
          Ok = isUInt<5>(Imm);
          break;
        }
        if (!Ok) {
          ErrInfo = "Invalid immediate";
          return false;
        }
      }
    }
  }

  return true;
}

static SimCC::CondCodes GetOppositeBranchCondition(SimCC::CondCodes CC)
{
  switch(CC) {
  default:
    llvm_unreachable("Unrecognized conditional branch");
  case SimCC::EQ:
    return SimCC::NE;
  case SimCC::NE:
    return SimCC::EQ;
  case SimCC::LT:
    return SimCC::GE;
  case SimCC::GT:
    return SimCC::LE;
  case SimCC::LE:
     return SimCC::GT;
  case SimCC::GE:
    return SimCC::LT;
  }
  llvm_unreachable("Invalid cond code");
}

static bool isUncondBranchOpcode(int Opc) {
  return Opc == SIM::B || Opc == SIM::BR;
}

static bool isCondBranchOpcode(int Opc) {
  return Opc == SIM::BEQ || Opc == SIM::BNE || Opc == SIM::BGT || Opc == SIM::BLE;
}

static bool isIndirectBranchOpcode(int Opc) {
  return Opc == SIM::B || Opc == SIM::BR;
}

const MCInstrDesc &SimInstrInfo::getBranchFromCond(SimCC::CondCodes CC) const {
  switch (CC) {
  default:
    llvm_unreachable("Unknown condition code!");
  case SimCC::EQ:
    return get(SIM::BEQ);
  case SimCC::NE:
    return get(SIM::BNE);
  case SimCC::LE:
    return get(SIM::BLE);
  case SimCC::GT:
    return get(SIM::BGT);
  // TODO: either add LEU/GTU conditions or delete it
  // case USimCC::LEU:
  //   return get(USim::BLEU);
  // case USimCC::GTU:
  //   return get(USim::BGTU);
  }
  llvm_unreachable("");
}

static SimCC::CondCodes getCondFromBranchOpcode(unsigned Opc) {
  switch (Opc) {
  default:
    return SimCC::INVALID;
  case SIM::BEQ:
    return SimCC::EQ;
  case SIM::BNE:
    return SimCC::NE;
  case SIM::BLE:
    return SimCC::LE;
  case SIM::BGT:
    return SimCC::GT;
  // TODO: either add LEU/GTU conditions or delete it
  // case Sim::BLEU:
  //   return SimCC::LEU;
  // case Sim::BGTU:
  //   return SimCC::GTU;
  }
  llvm_unreachable("");
}

static void parseCondBranch(MachineInstr *LastInst, MachineBasicBlock *&Target,
                            SmallVectorImpl<MachineOperand> &Cond) {
  assert(LastInst->getDesc().isConditionalBranch() &&
         "Unknown conditional branch");
  Target = LastInst->getOperand(2).getMBB();
  unsigned CC = getCondFromBranchOpcode(LastInst->getOpcode());
  Cond.push_back(MachineOperand::CreateImm(CC));
  Cond.push_back(LastInst->getOperand(0));
  Cond.push_back(LastInst->getOperand(1));
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
  assert((Cond.size() == 3 || Cond.size() == 0) &&
         "Sim branch conditions should have one component!");
  assert(!BytesAdded && "code size not handled");

  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(SIM::B)).addMBB(TBB);
    return 1;
  }

  // Conditional branch
  auto CC = static_cast<SimCC::CondCodes>(Cond[0].getImm());
  BuildMI(&MBB, DL, getBranchFromCond(CC))
    .addReg(Cond[1].getReg())
    .addReg(Cond[2].getReg())
    .addMBB(TBB);
  if (!FBB) {
    return 1;
  }

  BuildMI(&MBB, DL, get(SIM::B)).addMBB(FBB);
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

    if (I->getOpcode() != SIM::B
        && getCondFromBranchOpcode(I->getOpcode()) == SimCC::INVALID)
      break; // Not a branch

    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
}

bool SimInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() == 1 || Cond.size() == 3);
  SimCC::CondCodes CC = static_cast<SimCC::CondCodes>(Cond[0].getImm());
  Cond[0].setImm(GetOppositeBranchCondition(CC));
  return false;
}

void SimInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator I,
                                 const DebugLoc &DL, MCRegister DestReg,
                                 MCRegister SrcReg, bool KillSrc) const {
  if (SIM::GPRRegClass.contains(DestReg, SrcReg)) {
    BuildMI(MBB, I, DL, get(SIM::ADDi), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc))
        .addImm(0);
    return;
  }
  llvm_unreachable("");
}

void SimInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                    Register SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  MachineFrameInfo &MFI = MF->getFrameInfo();

  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOStore,
      MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  // STi must be agreed with isStoreToStackSlot()
  BuildMI(MBB, I, DL, get(SIM::STi))
      .addReg(SrcReg, getKillRegState(isKill))
      .addFrameIndex(FI)
      .addImm(0)
      .addMemOperand(MMO);
}

void SimInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                     Register DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  MachineFrameInfo &MFI = MF->getFrameInfo();

  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOLoad,
      MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  // LDi must be agreed with isLoadFromStackSlot()
  BuildMI(MBB, I, DL, get(SIM::LDi), DestReg)
      .addFrameIndex(FI)
      .addImm(0)
      .addMemOperand(MMO);
}
