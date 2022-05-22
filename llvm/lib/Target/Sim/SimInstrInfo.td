//===- SimInstrInfo.td - Target Description for Sim Target -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Sim implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Sim profiles and nodes
//===----------------------------------------------------------------------===//

// TODO: it's probably redundant
// def SDT_SimRet          : SDTypeProfile<0, 1, [SDTCisInt<0>]>;

def SDT_SimBrCC : SDTypeProfile<0, 4, [SDTCisSameAs<0, 1>,
                                       SDTCisVT<2, OtherVT>,
                                       SDTCisVT<3, OtherVT>]>;

def SDT_SimCall         : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;
def SDT_SimSelectCC     : SDTypeProfile<1, 5, [SDTCisVT<0, i32>,
                                               SDTCisVT<1, i32>,
                                               SDTCisSameAs<1, 2>,
                                               SDTCisSameAs<0, 3>,
                                               SDTCisSameAs<3, 4>,
                                               SDTCisVT<5, i32>]>;
def SDT_SimCallSeqStart : SDCallSeqStart<[ SDTCisVT<0, i32>,
                                           SDTCisVT<1, i32> ]>;
def SDT_SimCallSeqEnd   : SDCallSeqEnd<[ SDTCisVT<0, i32>,
                                         SDTCisVT<1, i32> ]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_SimCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_SimCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def SimCall       : SDNode<"SIMISD::CALL", SDT_SimCall,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                            SDNPVariadic]>;

def SimBrCC       : SDNode<"SIMISD::BR_CC", SDT_SimBrCC,
                           [SDNPHasChain]>;

def SimRet        : SDNode<"SIMISD::RET", SDTNone,
                           [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def SimSelectCC   : SDNode<"SIMISD::SELECT_CC", SDT_SimSelectCC>;

//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "SimInstrFormats.td"

//===----------------------------------------------------------------------===//
// Sim Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

// Addressing modes.
def ADDRrr : ComplexPattern<iPTR, 1, "SelectADDRrr", [], []>;
def ADDRri : ComplexPattern<iPTR, 1, "SelectADDRri", [frameindex], []>;

def LI16 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((uint64_t)N->getZExtValue() & 0xffff,
                                   SDLoc(N), MVT::i32);
}]>;

def HI16 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((uint64_t)N->getZExtValue() >> 16, SDLoc(N),
                                   MVT::i32);
}]>;

// TODO: can it be replaces with imm16?
def i32li16u : Operand<i32>, PatLeaf<(i32 imm), [{
  return ((N->getZExtValue() & 0xFFFFUL) == N->getZExtValue());
}], LI16> {}

def i32hi16 : Operand<i32>, PatLeaf<(i32 imm), [{
  return ((N->getZExtValue() & 0xFFFF0000UL) == N->getZExtValue());
}], HI16> {}

class ImmAsmOperand<string prefix, int width, string suffix> : AsmOperandClass {
  let Name = prefix # "Imm" # width # suffix;
  let DiagnosticType = !strconcat("Invalid", Name);
}

def SIMM16 : Operand<i32>, ImmLeaf<i32, [{return isInt<16>(Imm);}]> {
  let ParserMatchClass = ImmAsmOperand<"S", 16, "">;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeSImmOperand<16>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isInt<16>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_SIMM16";
  let OperandNamespace = "SimOp";
}

def UIMM16 : Operand<i32>, ImmLeaf<i32, [{return isUInt<16>(Imm);}]> {
  let ParserMatchClass = ImmAsmOperand<"U", 16, "">;
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isUInt<16>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_UIMM16";
  let OperandNamespace = "SimOp";
}

// from RISCV
// Constrained operand for the shift operations.
def uimm5 : Operand<i32>, ImmLeaf<i32, [{return isUInt<5>(Imm);}]> {
  let ParserMatchClass = ImmAsmOperand<"U", 5, "">;
  let OperandType = "OPERAND_UIMM5";
  let OperandNamespace = "SimOp";
}

def BTARGET16 : Operand<OtherVT> {
  let ParserMatchClass = ImmAsmOperand<"S", 16, "">;
  let PrintMethod = "printBranchOperand";
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeSImmOperand<16>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isInt<16>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_PCREL";
}

def CTARGET16 : Operand<i32> {
  let ParserMatchClass = ImmAsmOperand<"S", 16, "">;
  let PrintMethod = "printBranchOperand";
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeSImmOperand<16>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isInt<16>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_PCREL";
}

// From RISCV
def IsOrAdd: PatFrag<(ops node:$A, node:$B), (or node:$A, node:$B), [{
  return isOrEquivalentToAdd(N);
}]>;

//===----------------------------------------------------------------------===//
// Instructions specific format
//===----------------------------------------------------------------------===//

class ShiftImmIns<bits<8> op, string inststr, list<dag> pattern> : Instruction {
  let Namespace = "SIM";
  let DecoderNamespace = "SIM";
  let Size = 4;
  let hasSideEffects = 0;
  let mayLoad = 0;
  let mayStore = 0;

  field bits<32> Inst;

  bits<8> Opcode = op;
  bits<4>  r1;
  bits<4>  r2;
  bits<5>  imm;

  let Inst{31-24} = Opcode;
  let Inst{23-20} = r1;
  let Inst{19-16} = r2;
  let Inst{15-11} = imm;
  let Inst{10-0} = 0;

  let OutOperandList = (outs GPR:$r1);
  let InOperandList  = (ins GPR:$r2, uimm5:$imm);
  let AsmString   = inststr # " $r1 $r2 $imm";
  let Pattern     = pattern;

  let Itinerary = NoItinerary;
}

class CondBranch<bits<8> op, string inststr, list<dag> pattern> :
    SimInst<op, (outs), (ins GPR:$r1, GPR:$r2, BTARGET16:$r3_imm), inststr # " $r1 $r2 $r3_imm", pattern> {
  let hasSideEffects = 0;
  let mayLoad = 0;
  let mayStore = 0;
  let isTerminator = 1;
  let isBranch = 1;
}

//===----------------------------------------------------------------------===//
// Sim Instructions
//===----------------------------------------------------------------------===//

// No operation
def NOP     : SimInst<0, (outs), (ins), "NOP", []>;

def EXIT    : SimInst<0x01, (outs), (ins), "EXIT", []>;

// TODO: NEG is not implemented now, it is generated by "NEG rd r2 == LDi r1 0; SUB rd r1 r2"
def NEG    : SimInst<0x20, (outs GPR:$r1), (ins GPR:$r2), "NEG $r1 $r2", []> {
  let r3_imm = 0;
}
def ADD     : ALU_rr<0x21, "ADD", [(set GPR:$r1, (add GPR:$r2, GPR:$r3))]>;
def SUB     : ALU_rr<0x22, "SUB", [(set GPR:$r1, (sub GPR:$r2, GPR:$r3))]>;
def MUL     : ALU_rr<0x23, "MUL", [(set GPR:$r1, (mul GPR:$r2, GPR:$r3))]>;
def DIV     : ALU_rr<0x24, "DIV", [(set GPR:$r1, (sdiv GPR:$r2, GPR:$r3))]>;
def REM     : ALU_rr<0x25, "DIVrem", [(set GPR:$r1, (srem GPR:$r2, GPR:$r3))]>;
// def POW     : ALU_rr<0x26, "POW">;
// def NOT     : SimInst<0x40, (outs GPR:$r1), (ins GPR:$r2),
//                       "NOT $r1, $r2", [(set GPR:$r1, (not GPR:$r2))]>;
def AND     : ALU_rr<0x41, "AND", [(set GPR:$r1, (and GPR:$r2, GPR:$r3))]>;
def OR      : ALU_rr<0x42, "OR", [(set GPR:$r1, (or GPR:$r2, GPR:$r3))]>;
def XOR     : ALU_rr<0x43, "XOR", [(set GPR:$r1, (xor GPR:$r2, GPR:$r3))]>;
def SHL     : ALU_rr<0x44, "SHL", [(set GPR:$r1, (shl GPR:$r2, GPR:$r3))]>;
def SHR     : ALU_rr<0x45, "SHR", [(set GPR:$r1, (srl GPR:$r2, GPR:$r3))]>;
def SHRA    : ALU_rr<0x46, "SHRA", [(set GPR:$r1, (sra GPR:$r2, GPR:$r3))]>;

def ADDi     : ALU_ri<0x31, "ADDi", [(set GPR:$r1, (add GPR:$r2, SIMM16:$imm16))]>;
def SUBi     : ALU_ri<0x32, "SUBi", [(set GPR:$r1, (sub GPR:$r2, SIMM16:$imm16))]>;
def MULi     : ALU_ri<0x33, "MULi", [(set GPR:$r1, (mul GPR:$r2, SIMM16:$imm16))]>;
def DIVi     : ALU_ri<0x34, "DIVi", [(set GPR:$r1, (sdiv GPR:$r2, SIMM16:$imm16))]>;
def REMi     : ALU_ri<0x35, "DIV_REMi", [(set GPR:$r1, (srem GPR:$r2, SIMM16:$imm16))]>;
// def POWi     : ALU_ri<0x36, "POWi", [(set GPR:$r1, (add GPR:$r2, SIMM16:$imm16))]>;
// def POWi_    : ALU_ri<0x37, "POWi_", [(set GPR:$r1, (add GPR:$r2, SIMM16:$imm16))]>;
def ANDi     : ALU_ri<0x51, "ANDi", [(set GPR:$r1, (and GPR:$r2, SIMM16:$imm16))]>;
def ORi      : ALU_ri<0x52, "ORi", [(set GPR:$r1, (or GPR:$r2, SIMM16:$imm16))]>;
def XORi     : ALU_ri<0x53, "XORi", [(set GPR:$r1, (xor GPR:$r2, SIMM16:$imm16))]>;

//===----------------------------------------------------------------------===//
// Shifts definitions
//===----------------------------------------------------------------------===//

def SHLi     : ShiftImmIns<0x54, "SHLi", [(set GPR:$r1, (shl GPR:$r2, uimm5:$imm))]>;
def SHRi     : ShiftImmIns<0x55, "SHRi", [(set GPR:$r1, (srl GPR:$r2, uimm5:$imm))]>;
def SHRAi    : ShiftImmIns<0x56, "SHRAi", [(set GPR:$r1, (sra GPR:$r2, uimm5:$imm))]>;

//===----------------------------------------------------------------------===//
// Moves definitions
//===----------------------------------------------------------------------===//

def MOV      : SimInst<0x10, (outs GPR:$r1), (ins GPR:$r2), "MOV $r1 $r2", []> {
  let r3_imm = 0;
}
def MOVli    : SimInst<0x11, (outs GPR:$r1), (ins i32li16u:$imm16),
                       "MOVli $r1 $imm16", [(set GPR:$r1, i32li16u:$imm16)]> {
  let r2 = 0;
}
def MOVhi    : SimInst<0x12, (outs GPR:$r1), (ins i32li16u:$imm16),
                       "MOVhi $r1 $imm16", [(set GPR:$r1, i32li16u:$imm16)]> {
  let r2 = 0;
}

//===----------------------------------------------------------------------===//
// Branches definitions
//===----------------------------------------------------------------------===//

def B        : SimInst<0x02, (outs), (ins BTARGET16:$r3_imm), "B $r3_imm", [(br bb:$r3_imm)]> {
  let r1 = 0;
  let r2 = 0;
  let hasSideEffects = 0;
  let mayLoad = 0;
  let mayStore = 0;
  let isTerminator = 1;
  let isBranch = 1;
}

// TODO: add B.GTU and B.LEU
def BEQ      : CondBranch<0x06, "B.EQ", [(SimBrCC GPR:$r1, GPR:$r2, SETEQ, bb:$r3_imm)]>;
def BNE      : CondBranch<0x07, "B.NE", [(SimBrCC GPR:$r1, GPR:$r2, SETNE, bb:$r3_imm)]>;
def BGT      : CondBranch<0x08, "B.GT", [(SimBrCC GPR:$r1, GPR:$r2, SETGT, bb:$r3_imm)]>;
def BLE      : CondBranch<0x09, "B.LE", [(SimBrCC GPR:$r1, GPR:$r2, SETLE, bb:$r3_imm)]>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCall = 1, isBranch = 1, isTerminator = 1 in {
def BR : SimInst<0x05, (outs), (ins GPR:$r1), "BR $r1",
                 [(brind GPR:$r1)]>;
def BL : SimInst<0x03, (outs GPR:$r1), (ins CTARGET16:$r3_imm),
                 "BL $r1 $r3_imm", []>;
// BLR needed for indirect pseudo-call only
def BLR : SimInst<0x0, (outs GPR:$r1), (ins GPR:$r2),
                  "BLR $r1 $r2", []>;
}

//===----------------------------------------------------------------------===//
// Loads definitions
//===----------------------------------------------------------------------===//

def LD       : SimInst<0x70, (outs GPR:$r1), (ins GPR:$r2, GPR:$r3_imm),
                       "LD $r1 $r2 $r3_imm",
                       [(set GPR:$r1, (load (add ADDRrr:$r2, GPR:$r3_imm)))]> {
  let hasSideEffects = 0;
  let mayLoad = 1;
  let mayStore = 0;
}

def LDi      : SimInst<0x72, (outs GPR:$r1), (ins GPR:$r2, SIMM16:$r3_imm),
                       "LDi $r1 $r2 $r3_imm",
                       [(set GPR:$r1, (load (IsOrAdd ADDRri:$r2, SIMM16:$r3_imm)))]> {
  let hasSideEffects = 0;
  let mayLoad = 1;
  let mayStore = 0;
}

def : Pat<(i32 (load ADDRrr:$r2)), (LDi ADDRrr:$r2, 0)>;

//===----------------------------------------------------------------------===//
// Stores definitions
//===----------------------------------------------------------------------===//

def ST       : SimInst<0x71, (outs), (ins GPR:$r1, GPR:$r2, GPR:$r3_imm),
                       "ST $r1 $r2 $r3_imm",
                       [(store (i32 GPR:$r1), (add ADDRrr:$r2, GPR:$r3_imm))]> {
  let hasSideEffects = 0;
  let mayLoad = 0;
  let mayStore = 1;
}

def STi      : SimInst<0x73, (outs), (ins GPR:$r1, GPR:$r2, SIMM16:$r3_imm),
                       "STi $r1 $r2 $r3_imm",
                       [(store (i32 GPR:$r1), (IsOrAdd ADDRri:$r2, SIMM16:$r3_imm))]> {
  let hasSideEffects = 0;
  let mayLoad = 0;
  let mayStore = 1;
}

def : Pat<(store (i32 GPR:$r1), ADDRri:$r2),
          (STi GPR:$r1, ADDRri:$r2, 0)>;
def : Pat<(store (i32 GPR:$r1), ADDRrr:$r2),
          (STi GPR:$r1, ADDRrr:$r2, 0)>;

//===----------------------------------------------------------------------===//
// Pseudo Instructions
//===----------------------------------------------------------------------===//

// GP == R2
// GP changed to R0 for compatibility with emulator
let Defs = [R0], Uses = [R0] in {
def ADJCALLSTACKDOWN : SimPseudoInst<(outs), (ins i32imm:$amt, i32imm:$amt2),
                               "# ADJCALLSTACKDOWN $amt $amt2",
                               [(callseq_start timm:$amt, timm:$amt2)]>;
def ADJCALLSTACKUP : SimPseudoInst<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                            "# ADJCALLSTACKUP $amt1",
                            [(callseq_end timm:$amt1, timm:$amt2)]>;
}

// RA == R0
// RA changed to R1 for compatibility with emulator
def PseudoRET : SimPseudoInst<(outs), (ins), "# PseudoRET",
                              [(SimRet)]>,
                              PseudoInstExpansion<(BR R1)> {
  let isBarrier = 1;
  let isReturn = 1;
  let isTerminator = 1;
}

let isCall = 1, Defs = [R1] in
def PseudoCALL : SimPseudoInst<(outs), (ins CTARGET16:$func), "# PseudoCALL",
                               [(SimCall tglobaladdr:$func)]>,
                               PseudoInstExpansion<(BL R1, CTARGET16:$func)>;

// TODO: implement it
let isCall = 1, Defs = [R1] in
def PseudoCALLInd : SimPseudoInst<(outs), (ins GPR:$rs1), "# PseudoCALLInd",
                    [(SimCall GPR:$rs1)]>,
                    PseudoInstExpansion<(BLR R1, GPR:$rs1)>;

let usesCustomInserter = 1 in
def PseudoSELECT_CC : SimPseudoInst<(outs GPR:$dst),
                                    (ins GPR:$lhs, GPR:$rhs, GPR:$truev, GPR:$falsev, i32imm:$Cond),
                                    "# PseudoSELECT_CC",
                                    [(set GPR:$dst,
                                     (SimSelectCC GPR:$lhs, GPR:$rhs,
                                      GPR:$truev, GPR:$falsev, imm:$Cond))]>;

// i16 imm
def : Pat<(i32 i32li16u:$imm16), (MOVli imm:$imm16)>;
// i32 imm
def : Pat<(i32 imm:$imm32), (ORi (MOVhi (HI16 imm:$imm32)), (LI16 imm:$imm32))>;
