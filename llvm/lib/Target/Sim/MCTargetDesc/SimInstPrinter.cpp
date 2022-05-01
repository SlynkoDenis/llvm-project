//===-- SimInstPrinter.cpp - Convert Sim MCInst to assembly syntax --0000---==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This class prints an Sim MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "SimInstPrinter.h"
#include "Sim.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

// The generated AsmMatcher SimGenAsmWriter uses "Sim" as the target
// namespace. But SIM backend uses "SP" as its namespace.
// namespace llvm {
// namespace Sim {
//   using namespace SP;
// }
// }

#define GET_INSTRUCTION_NAME
#define PRINT_ALIAS_INSTR
#include "SimGenAsmWriter.inc"

void SimInstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const
{
  // changed for compatibility with emulator
  // OS << '%' << StringRef(getRegisterName(RegNo)).lower();
  OS << StringRef(getRegisterName(RegNo)).lower();
}

void SimInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                               StringRef Annot, const MCSubtargetInfo &STI,
                               raw_ostream &O) {
  if (!printAliasInstr(MI, Address, O)) {
    printInstruction(MI, Address, O);
  }
  printAnnotation(O, Annot);
}

void SimInstPrinter::printOperand(const MCInst *MI, uint64_t opNum, raw_ostream &OS) {
  const MCOperand &MO = MI->getOperand(opNum);

  if (MO.isReg()) {
    printRegName(OS, MO.getReg());
    return;
  }

  if (MO.isImm()) {
    OS << MO.getImm();
    return;
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(OS, &MAI);
}

void SimInstPrinter::printBranchOperand(const MCInst *MI, uint64_t Address,
                                        unsigned OpNum, raw_ostream &OS) {
  const MCOperand &MO = MI->getOperand(OpNum);
  if (!MO.isImm()) {
    return printOperand(MI, OpNum, OS);
  }

  if (PrintBranchImmAsAddress) {
    uint32_t Target = Address + MO.getImm();
    OS << formatHex(static_cast<uint64_t>(Target));
  } else {
    OS << MO.getImm();
  }
}

// void SimInstPrinter::printMemOperand(const MCInst *MI, int opNum,
//                                        const MCSubtargetInfo &STI,
//                                        raw_ostream &O, const char *Modifier) {
//   // If this is an ADD operand, emit it like normal operands.
//   if (Modifier && !strcmp(Modifier, "arith")) {
//     printOperand(MI, opNum, STI, O);
//     O << ", ";
//     printOperand(MI, opNum + 1, STI, O);
//     return;
//   }

//   const MCOperand &Op1 = MI->getOperand(opNum);
//   const MCOperand &Op2 = MI->getOperand(opNum + 1);

//   bool PrintedFirstOperand = false;
//   if (Op1.isReg() && Op1.getReg() != SP::G0) {
//     printOperand(MI, opNum, STI, O);
//     PrintedFirstOperand = true;
//   }

//   // Skip the second operand iff it adds nothing (literal 0 or %g0) and we've
//   // already printed the first one
//   const bool SkipSecondOperand =
//       PrintedFirstOperand && ((Op2.isReg() && Op2.getReg() == SP::G0) ||
//                               (Op2.isImm() && Op2.getImm() == 0));

//   if (!SkipSecondOperand) {
//     if (PrintedFirstOperand)
//       O << '+';
//     printOperand(MI, opNum + 1, STI, O);
//   }
// }

void SimInstPrinter::printCCOperand(const MCInst *MI, int opNum,
                                    const MCSubtargetInfo &STI,
                                    raw_ostream &O) {
  const MCOperand Operand = MI->getOperand(opNum);
  assert(Operand.isImm() && "Operand must be Immediate");

  int CC = Operand.getImm();
  O << CC;
  // TODO: enable it
//   switch (MI->getOpcode()) {
//   default: break;
//   case SP::FBCOND:
//   case SP::FBCONDA:
//   case SP::BPFCC:
//   case SP::BPFCCA:
//   case SP::BPFCCNT:
//   case SP::BPFCCANT:
//   case SP::MOVFCCrr:  case SP::V9MOVFCCrr:
//   case SP::MOVFCCri:  case SP::V9MOVFCCri:
//   case SP::FMOVS_FCC: case SP::V9FMOVS_FCC:
//   case SP::FMOVD_FCC: case SP::V9FMOVD_FCC:
//   case SP::FMOVQ_FCC: case SP::V9FMOVQ_FCC:
//     // Make sure CC is a fp conditional flag.
//     CC = (CC < 16) ? (CC + 16) : CC;
//     break;
//   case SP::CBCOND:
//   case SP::CBCONDA:
//     // Make sure CC is a cp conditional flag.
//     CC = (CC < 32) ? (CC + 32) : CC;
//     break;
//   }
//   O << SPARCCondCodeToString((SPCC::CondCodes)CC);
}

bool SimInstPrinter::printGetPCX(const MCInst *MI, unsigned opNum,
                                   const MCSubtargetInfo &STI,
                                   raw_ostream &O) {
  llvm_unreachable("FIXME: Implement SimInstPrinter::printGetPCX.");
  return true;
}

void SimInstPrinter::printMembarTag(const MCInst *MI, int opNum,
                                      const MCSubtargetInfo &STI,
                                      raw_ostream &O) {
  llvm_unreachable("FIXME: Implement SimInstPrinter::printMembarTag.");
  return;
  static const char *const TagNames[] = {
      "#LoadLoad",  "#StoreLoad", "#LoadStore", "#StoreStore",
      "#Lookaside", "#MemIssue",  "#Sync"};

  unsigned Imm = MI->getOperand(opNum).getImm();

  if (Imm > 127) {
    O << Imm;
    return;
  }

  bool First = true;
  for (unsigned i = 0; i < sizeof(TagNames) / sizeof(char *); i++) {
    if (Imm & (1 << i)) {
      O << (First ? "" : " | ") << TagNames[i];
      First = false;
    }
  }
}
