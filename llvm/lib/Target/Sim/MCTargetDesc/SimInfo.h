#ifndef LLVM_LIB_TARGET_SIM_MCTARGETDESC_SIMINFO_H
#define LLVM_LIB_TARGET_SIM_MCTARGETDESC_SIMINFO_H

#include "llvm/MC/MCInstrDesc.h"

namespace llvm {

namespace SimCC {
enum CondCodes {
  EQ,
  NE,
  LT,
  GT,
  LE,
  GE,
  INVALID,
};

CondCodes getOppositeBranchCondition(CondCodes);

// TODO: do u need BRCondCode custom enum?
enum BRCondCode {
  BREQ = 0x0,
};
} // end namespace SimCC

namespace SimOp {
enum OperandType : unsigned {
  OPERAND_SIMM16 = MCOI::OPERAND_FIRST_TARGET,
  OPERAND_UIMM16,
  OPERAND_UIMM5,
};
} // namespace SimOp

} // end namespace llvm

#endif  // LLVM_LIB_TARGET_SIM_MCTARGETDESC_SIMINFO_H
