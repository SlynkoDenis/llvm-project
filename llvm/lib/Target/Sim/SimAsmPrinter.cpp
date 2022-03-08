//===-- SimAsmPrinter.cpp - Sim LLVM assembly writer ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format Sim assembly language.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SimInstPrinter.h"
#include "MCTargetDesc/SimMCExpr.h"
#include "MCTargetDesc/SimTargetStreamer.h"
#include "Sim.h"
#include "SimInstrInfo.h"
#include "SimTargetMachine.h"
#include "TargetInfo/SimTargetInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {
  class SimAsmPrinter : public AsmPrinter {
    SimTargetStreamer &getTargetStreamer() {
      return static_cast<SimTargetStreamer &>(
          *OutStreamer->getTargetStreamer());
    }
  public:
    explicit SimAsmPrinter(TargetMachine &TM,
                             std::unique_ptr<MCStreamer> Streamer)
        : AsmPrinter(TM, std::move(Streamer)) {}

    StringRef getPassName() const override { return "Sim Assembly Printer"; }

    void printOperand(const MachineInstr *MI, int opNum, raw_ostream &OS);
    void printMemOperand(const MachineInstr *MI, int opNum, raw_ostream &OS,
                         const char *Modifier = nullptr);

    void emitFunctionBodyStart() override;
    void emitInstruction(const MachineInstr *MI) override;

    static const char *getRegisterName(unsigned RegNo) {
      return SimInstPrinter::getRegisterName(RegNo);
    }

    bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                         const char *ExtraCode, raw_ostream &O) override;
    bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                               const char *ExtraCode, raw_ostream &O) override;

    void LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                   const MCSubtargetInfo &STI);

  };
} // end of anonymous namespace

static MCOperand createSimMCOperand(SimMCExpr::VariantKind Kind,
                                      MCSymbol *Sym, MCContext &OutContext) {
  const MCSymbolRefExpr *MCSym = MCSymbolRefExpr::create(Sym,
                                                         OutContext);
  const SimMCExpr *expr = SimMCExpr::create(Kind, MCSym, OutContext);
  return MCOperand::createExpr(expr);

}
static MCOperand createPCXCallOP(MCSymbol *Label,
                                 MCContext &OutContext) {
  return createSimMCOperand(SimMCExpr::VK_Sim_WDISP30, Label, OutContext);
}

static MCOperand createPCXRelExprOp(SimMCExpr::VariantKind Kind,
                                    MCSymbol *GOTLabel, MCSymbol *StartLabel,
                                    MCSymbol *CurLabel,
                                    MCContext &OutContext)
{
  const MCSymbolRefExpr *GOT = MCSymbolRefExpr::create(GOTLabel, OutContext);
  const MCSymbolRefExpr *Start = MCSymbolRefExpr::create(StartLabel,
                                                         OutContext);
  const MCSymbolRefExpr *Cur = MCSymbolRefExpr::create(CurLabel,
                                                       OutContext);

  const MCBinaryExpr *Sub = MCBinaryExpr::createSub(Cur, Start, OutContext);
  const MCBinaryExpr *Add = MCBinaryExpr::createAdd(GOT, Sub, OutContext);
  const SimMCExpr *expr = SimMCExpr::create(Kind,
                                                Add, OutContext);
  return MCOperand::createExpr(expr);
}

static void EmitCall(MCStreamer &OutStreamer,
                     MCOperand &Callee,
                     const MCSubtargetInfo &STI)
{
  MCInst CallInst;
  CallInst.setOpcode(SIM::CALL);
  CallInst.addOperand(Callee);
  OutStreamer.emitInstruction(CallInst, STI);
}

static void EmitBinary(MCStreamer &OutStreamer, unsigned Opcode,
                       MCOperand &RS1, MCOperand &Src2, MCOperand &RD,
                       const MCSubtargetInfo &STI)
{
  MCInst Inst;
  Inst.setOpcode(Opcode);
  Inst.addOperand(RD);
  Inst.addOperand(RS1);
  Inst.addOperand(Src2);
  OutStreamer.emitInstruction(Inst, STI);
}

static void EmitOR(MCStreamer &OutStreamer,
                   MCOperand &RS1, MCOperand &Imm, MCOperand &RD,
                   const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SIM::ORi, RS1, Imm, RD, STI);
}

static void EmitADD(MCStreamer &OutStreamer,
                    MCOperand &RS1, MCOperand &RS2, MCOperand &RD,
                    const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SIM::ADD, RS1, RS2, RD, STI);
}

static void EmitSHL(MCStreamer &OutStreamer,
                    MCOperand &RS1, MCOperand &Imm, MCOperand &RD,
                    const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SIM::SHLi, RS1, Imm, RD, STI);
}


static void EmitHiLo(MCStreamer &OutStreamer,  MCSymbol *GOTSym,
                     SimMCExpr::VariantKind HiKind,
                     SimMCExpr::VariantKind LoKind,
                     MCOperand &RD,
                     MCContext &OutContext,
                     const MCSubtargetInfo &STI) {

  MCOperand hi = createSimMCOperand(HiKind, GOTSym, OutContext);
  MCOperand lo = createSimMCOperand(LoKind, GOTSym, OutContext);
  EmitSETHI(OutStreamer, hi, RD, STI);
  EmitOR(OutStreamer, RD, lo, RD, STI);
}

void SimAsmPrinter::LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                                const MCSubtargetInfo &STI)
{
  MCSymbol *GOTLabel =
    OutContext.getOrCreateSymbol(Twine("_GLOBAL_OFFSET_TABLE_"));

  const MachineOperand &MO = MI->getOperand(0);
  assert(MO.getReg() != SIM::O7 &&
         "%o7 is assigned as destination for getpcx!");

  MCOperand MCRegOP = MCOperand::createReg(MO.getReg());


  if (!isPositionIndependent()) {
    // Just load the address of GOT to MCRegOP.
    switch(TM.getCodeModel()) {
    default:
      llvm_unreachable("Unsupported absolute code model");
    case CodeModel::Small:
      EmitHiLo(*OutStreamer, GOTLabel,
               SimMCExpr::VK_Sim_HI, SimMCExpr::VK_Sim_LO,
               MCRegOP, OutContext, STI);
      break;
    case CodeModel::Medium: {
      EmitHiLo(*OutStreamer, GOTLabel,
               SimMCExpr::VK_Sim_H44, SimMCExpr::VK_Sim_M44,
               MCRegOP, OutContext, STI);
      MCOperand imm = MCOperand::createExpr(MCConstantExpr::create(12,
                                                                   OutContext));
      EmitSHL(*OutStreamer, MCRegOP, imm, MCRegOP, STI);
      MCOperand lo = createSimMCOperand(SimMCExpr::VK_Sim_L44,
                                          GOTLabel, OutContext);
      EmitOR(*OutStreamer, MCRegOP, lo, MCRegOP, STI);
      break;
    }
    case CodeModel::Large: {
      EmitHiLo(*OutStreamer, GOTLabel,
               SimMCExpr::VK_Sim_HH, SimMCExpr::VK_Sim_HM,
               MCRegOP, OutContext, STI);
      MCOperand imm = MCOperand::createExpr(MCConstantExpr::create(32,
                                                                   OutContext));
      EmitSHL(*OutStreamer, MCRegOP, imm, MCRegOP, STI);
      // Use register %o7 to load the lower 32 bits.
      MCOperand RegO7 = MCOperand::createReg(SIM::O7);
      EmitHiLo(*OutStreamer, GOTLabel,
               SimMCExpr::VK_Sim_HI, SimMCExpr::VK_Sim_LO,
               RegO7, OutContext, STI);
      EmitADD(*OutStreamer, MCRegOP, RegO7, MCRegOP, STI);
    }
    }
    return;
  }

  MCSymbol *StartLabel = OutContext.createTempSymbol();
  MCSymbol *EndLabel   = OutContext.createTempSymbol();
  MCSymbol *SethiLabel = OutContext.createTempSymbol();

  MCOperand RegO7   = MCOperand::createReg(SIM::O7);

  // <StartLabel>:
  //   call <EndLabel>
  // <SethiLabel>:
  //     sethi %hi(_GLOBAL_OFFSET_TABLE_+(<SethiLabel>-<StartLabel>)), <MO>
  // <EndLabel>:
  //   or  <MO>, %lo(_GLOBAL_OFFSET_TABLE_+(<EndLabel>-<StartLabel>))), <MO>
  //   add <MO>, %o7, <MO>

  OutStreamer->emitLabel(StartLabel);
  MCOperand Callee =  createPCXCallOP(EndLabel, OutContext);
  EmitCall(*OutStreamer, Callee, STI);
  OutStreamer->emitLabel(SethiLabel);
  MCOperand hiImm = createPCXRelExprOp(SimMCExpr::VK_Sim_PC22,
                                       GOTLabel, StartLabel, SethiLabel,
                                       OutContext);
  EmitSETHI(*OutStreamer, hiImm, MCRegOP, STI);
  OutStreamer->emitLabel(EndLabel);
  MCOperand loImm = createPCXRelExprOp(SimMCExpr::VK_Sim_PC10,
                                       GOTLabel, StartLabel, EndLabel,
                                       OutContext);
  EmitOR(*OutStreamer, MCRegOP, loImm, MCRegOP, STI);
  EmitADD(*OutStreamer, MCRegOP, RegO7, MCRegOP, STI);
}

void SimAsmPrinter::emitInstruction(const MachineInstr *MI) {

  switch (MI->getOpcode()) {
  default: break;
  case TargetOpcode::DBG_VALUE:
    // FIXME: Debug Value.
    return;
  // TODO: implement
  // case SIM::RET:
  case SIM::GETPCX:
    LowerGETPCXAndEmitMCInsts(MI, getSubtargetInfo());
    return;
  }
  MachineBasicBlock::const_instr_iterator I = MI->getIterator();
  MachineBasicBlock::const_instr_iterator E = MI->getParent()->instr_end();
  do {
    MCInst TmpInst;
    LowerSimMachineInstrToMCInst(&*I, TmpInst, *this);
    EmitToStreamer(*OutStreamer, TmpInst);
  } while ((++I != E) && I->isInsideBundle()); // Delay slot check.
}

void SimAsmPrinter::emitFunctionBodyStart() {
  if (!MF->getSubtarget<SimSubtarget>().is64Bit())
    return;

  const MachineRegisterInfo &MRI = MF->getRegInfo();
  const unsigned globalRegs[] = { SIM::G2, SIM::G3, SIM::G6, SIM::G7, 0 };
  for (unsigned i = 0; globalRegs[i] != 0; ++i) {
    unsigned reg = globalRegs[i];
    if (MRI.use_empty(reg))
      continue;

    if  (reg == SIM::G6 || reg == SIM::G7)
      getTargetStreamer().emitSimRegisterIgnore(reg);
    else
      getTargetStreamer().emitSimRegisterScratch(reg);
  }
}

void SimAsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                   raw_ostream &O) {
  const DataLayout &DL = getDataLayout();
  const MachineOperand &MO = MI->getOperand (opNum);
  SimMCExpr::VariantKind TF = (SimMCExpr::VariantKind) MO.getTargetFlags();

#ifndef NDEBUG
  // Verify the target flags.
  if (MO.isGlobal() || MO.isSymbol() || MO.isCPI()) {
    if (MI->getOpcode() == SIM::CALL)
      assert(TF == SimMCExpr::VK_Sim_None &&
             "Cannot handle target flags on call address");
    else if (MI->getOpcode() == SIM::SETHIi || MI->getOpcode() == SIM::SETHIXi)
      assert((TF == SimMCExpr::VK_Sim_HI
              || TF == SimMCExpr::VK_Sim_H44
              || TF == SimMCExpr::VK_Sim_HH
              || TF == SimMCExpr::VK_Sim_LM
              || TF == SimMCExpr::VK_Sim_TLS_GD_HI22
              || TF == SimMCExpr::VK_Sim_TLS_LDM_HI22
              || TF == SimMCExpr::VK_Sim_TLS_LDO_HIX22
              || TF == SimMCExpr::VK_Sim_TLS_IE_HI22
              || TF == SimMCExpr::VK_Sim_TLS_LE_HIX22) &&
             "Invalid target flags for address operand on sethi");
    else if (MI->getOpcode() == SIM::TLS_CALL)
      assert((TF == SimMCExpr::VK_Sim_None
              || TF == SimMCExpr::VK_Sim_TLS_GD_CALL
              || TF == SimMCExpr::VK_Sim_TLS_LDM_CALL) &&
             "Cannot handle target flags on tls call address");
    else if (MI->getOpcode() == SIM::TLS_ADDrr)
      assert((TF == SimMCExpr::VK_Sim_TLS_GD_ADD
              || TF == SimMCExpr::VK_Sim_TLS_LDM_ADD
              || TF == SimMCExpr::VK_Sim_TLS_LDO_ADD
              || TF == SimMCExpr::VK_Sim_TLS_IE_ADD) &&
             "Cannot handle target flags on add for TLS");
    else if (MI->getOpcode() == SIM::TLS_LDrr)
      assert(TF == SimMCExpr::VK_Sim_TLS_IE_LD &&
             "Cannot handle target flags on ld for TLS");
    else if (MI->getOpcode() == SIM::TLS_LDXrr)
      assert(TF == SimMCExpr::VK_Sim_TLS_IE_LDX &&
             "Cannot handle target flags on ldx for TLS");
    else if (MI->getOpcode() == SIM::XORri || MI->getOpcode() == SIM::XORXri)
      assert((TF == SimMCExpr::VK_Sim_TLS_LDO_LOX10
              || TF == SimMCExpr::VK_Sim_TLS_LE_LOX10) &&
             "Cannot handle target flags on xor for TLS");
    else
      assert((TF == SimMCExpr::VK_Sim_LO
              || TF == SimMCExpr::VK_Sim_M44
              || TF == SimMCExpr::VK_Sim_L44
              || TF == SimMCExpr::VK_Sim_HM
              || TF == SimMCExpr::VK_Sim_TLS_GD_LO10
              || TF == SimMCExpr::VK_Sim_TLS_LDM_LO10
              || TF == SimMCExpr::VK_Sim_TLS_IE_LO10 ) &&
             "Invalid target flags for small address operand");
  }
#endif


  bool CloseParen = SimMCExpr::printVariantKind(O, TF);

  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    O << "%" << StringRef(getRegisterName(MO.getReg())).lower();
    break;

  case MachineOperand::MO_Immediate:
    O << MO.getImm();
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MO.getMBB()->getSymbol()->print(O, MAI);
    return;
  case MachineOperand::MO_GlobalAddress:
    PrintSymbolOperand(MO, O);
    break;
  case MachineOperand::MO_BlockAddress:
    O <<  GetBlockAddressSymbol(MO.getBlockAddress())->getName();
    break;
  case MachineOperand::MO_ExternalSymbol:
    O << MO.getSymbolName();
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    O << DL.getPrivateGlobalPrefix() << "CPI" << getFunctionNumber() << "_"
      << MO.getIndex();
    break;
  case MachineOperand::MO_Metadata:
    MO.getMetadata()->printAsOperand(O, MMI->getModule());
    break;
  default:
    llvm_unreachable("<unknown operand type>");
  }
  if (CloseParen) O << ")";
}

void SimAsmPrinter::printMemOperand(const MachineInstr *MI, int opNum,
                                      raw_ostream &Os, const char *Modifier) {
  printOperand(MI, opNum, Os);

  // If this is an ADD operand, emit it like normal operands.
  if (Modifier && !strcmp(Modifier, "arith")) {
    Os << ", ";
    printOperand(MI, opNum+1, Os);
    return;
  }

  if (MI->getOperand(opNum+1).isReg() &&
      MI->getOperand(opNum+1).getReg() == SIM::G0)
    return;   // don't print "+%g0"
  if (MI->getOperand(opNum+1).isImm() &&
      MI->getOperand(opNum+1).getImm() == 0)
    return;   // don't print "+0"

  Os << "+";
  printOperand(MI, opNum+1, Os);
}

/// PrintAsmOperand - Print out an operand for an inline asm expression.
///
bool SimAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                      const char *ExtraCode,
                                      raw_ostream &Os) {
  if (ExtraCode && ExtraCode[0]) {
    if (ExtraCode[1] != 0) return true; // Unknown modifier.

    switch (ExtraCode[0]) {
    default:
      // See if this is a generic print operand
      return AsmPrinter::PrintAsmOperand(MI, OpNo, ExtraCode, Os);
    case 'f':
    case 'r':
     break;
    }
  }

  printOperand(MI, OpNo, Os);

  return false;
}

bool SimAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                            unsigned OpNo,
                                            const char *ExtraCode,
                                            raw_ostream &Os) {
  if (ExtraCode && ExtraCode[0])
    return true;  // Unknown modifier

  Os << '[';
  printMemOperand(MI, OpNo, Os);
  Os << ']';

  return false;
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSimAsmPrinter() {
  llvm::RegisterAsmPrinter<SimAsmPrinter> X(getTheSimTarget());
}
