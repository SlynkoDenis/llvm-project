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

#include "Sim.h"

static llvm::MCOperand lowerSymbolOperand(const llvm::MachineOperand &MO, llvm::MCSymbol *Sym,
                                          const llvm::AsmPrinter &AP) {
  llvm::MCContext &Ctx = AP.OutContext;

  const llvm::MCExpr *ME =
      llvm::MCSymbolRefExpr::create(Sym, llvm::MCSymbolRefExpr::VK_None, Ctx);

  if (!MO.isJTI() && !MO.isMBB() && MO.getOffset())
    ME = llvm::MCBinaryExpr::createAdd(
        ME, llvm::MCConstantExpr::create(MO.getOffset(), Ctx), Ctx);

  return llvm::MCOperand::createExpr(ME);
}

bool llvm::lowerSimMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                        AsmPrinter &AP) {
  OutMI.setOpcode(MI->getOpcode());

  for (const MachineOperand &MO : MI->operands()) {
    MCOperand MCOp;
    if (lowerSimMachineOperandToMCOperand(MO, MCOp, AP))
      OutMI.addOperand(MCOp);
  }
  return false;
}

bool llvm::lowerSimMachineOperandToMCOperand(const MachineOperand &MO,
                                             MCOperand &MCOp, const AsmPrinter &AP) {
  switch (MO.getType()) {
  default:
    report_fatal_error("LowerUSimMachineInstrToMCInst: unknown operand type");
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands.
    if (MO.isImplicit())
      return false;
    MCOp = MCOperand::createReg(MO.getReg());
    break;
  case MachineOperand::MO_RegisterMask:
    // Regmasks are like implicit defs.
    return false;
  case MachineOperand::MO_Immediate:
    MCOp = MCOperand::createImm(MO.getImm());
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MCOp = lowerSymbolOperand(MO, MO.getMBB()->getSymbol(), AP);
    break;
  case MachineOperand::MO_GlobalAddress:
    MCOp = lowerSymbolOperand(MO, AP.getSymbolPreferLocal(*MO.getGlobal()), AP);
    break;
  case MachineOperand::MO_BlockAddress:
    MCOp = lowerSymbolOperand(
        MO, AP.GetBlockAddressSymbol(MO.getBlockAddress()), AP);
    break;
  case MachineOperand::MO_ExternalSymbol:
    MCOp = lowerSymbolOperand(
        MO, AP.GetExternalSymbolSymbol(MO.getSymbolName()), AP);
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    MCOp = lowerSymbolOperand(MO, AP.GetCPISymbol(MO.getIndex()), AP);
    break;
  case MachineOperand::MO_JumpTableIndex:
    MCOp = lowerSymbolOperand(MO, AP.GetJTISymbol(MO.getIndex()), AP);
    break;
  }
  return true;
}

// FunctionPass *llvm::createSimISelDag(SimTargetMachine &TM,
//                                      CodeGenOpt::Level OptLevel) {
// }
