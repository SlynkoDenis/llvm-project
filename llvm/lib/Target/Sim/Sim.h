//===-- Sim.h - Top-level interface for Sim representation ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in
// the LLVM Sim back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SIM_SIM_H
#define LLVM_LIB_TARGET_SIM_SIM_H

#include "MCTargetDesc/SimMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/Pass.h"

namespace llvm {
class SimTargetMachine;

bool lowerSimMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                  AsmPrinter &AP);
bool lowerSimMachineOperandToMCOperand(const MachineOperand &MO,
                                       MCOperand &MCOp, const AsmPrinter &AP);

FunctionPass *createSimISelDag(SimTargetMachine &TM,
                               CodeGenOpt::Level OptLevel);

namespace SIM {
enum {
  // changed for compatibility with emulator
  GP = SIM::R0,
  RA = SIM::R1,
  SP = SIM::R2,
  FP = SIM::R3,
  BP = SIM::R4,
  // Similar to RISC-V
  // RA = SIM::R0,
  // SP = SIM::R1,
  // GP = SIM::R2,
  // FP = SIM::R3,
  // BP = SIM::R4,
};
} // end namespace SIM;

} // end namespace llvm;

#endif  // LLVM_LIB_TARGET_SIM_SIM_H
