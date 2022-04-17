#ifndef LLVM_LIB_TARGET_SIM_MCTARGETDESC_SIMINFO_H
#define LLVM_LIB_TARGET_SIM_MCTARGETDESC_SIMINFO_H

#include "llvm/MC/MCInstrDesc.h"

namespace llvm {

namespace SimCC {
enum CondCodes {
  EQ,
  NE,
  LE,
  GT,
  LEU,
  GTU,
  INVALID,
};

CondCodes getOppositeBranchCondition(CondCodes);

enum BRCondCode {
  BREQ = 0x0,
};
} // end namespace SimCC

namespace SimOp {
enum OperandType : unsigned {
  OPERAND_SIMM16 = MCOI::OPERAND_FIRST_TARGET,
  OPERAND_UIMM16,
};
} // namespace USimOp

} // end namespace llvm

#endif
