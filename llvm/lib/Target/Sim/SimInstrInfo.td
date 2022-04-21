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

// Return
def SimRet : SDNode<"SIMISD::RET", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def SDT_SimBrCC : SDTypeProfile<0, 4, [SDTCisSameAs<0, 1>,
                                       SDTCisVT<2, OtherVT>,
                                       SDTCisVT<3, OtherVT>]>;

def SDT_SimCall         : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;
def SDT_SimCallSeqStart : SDCallSeqStart<[ SDTCisVT<0, i32>,
                                           SDTCisVT<1, i32> ]>;
def SDT_SimCallSeqEnd   : SDCallSeqEnd<[ SDTCisVT<0, i32>,
                                         SDTCisVT<1, i32> ]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_SimCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_SimCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

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
  let RenderMethod = "addImmOperands";
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
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeUImmOperand<16>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isUInt<16>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_UIMM16";
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

//===----------------------------------------------------------------------===//
// Instructions specific format
//===----------------------------------------------------------------------===//

// class Branches<bits<8> op, string instr_asm, RegisterClass RC>
//     : SimInst<op, (outs), (ins RC:$r1),
//               !strconcat(instr_asm, "\t$r1"), [(brind RC:$r1)]> {
//   let isBranch = 1;
//   let isTerminator = 1;
//   let isBarrier = 1;
//   let hasDelaySlot = 1;
//   let isIndirectBranch = 1;
// }

// // Return instruction
// class RetBase<RegisterClass RC>: Branches<0x3c, "RET", RC> {
//   let isReturn = 1;
//   let isCodeGenOnly = 1;
//   let hasCtrlDep = 1;
//   let hasExtraSrcRegAllocReq = 1;
// }

//===----------------------------------------------------------------------===//
// Instruction definition
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Sim Instructions
//===----------------------------------------------------------------------===//

// def RET     : RetBase<CPURegs>;

/// No operation
def NOP     : SimInst<0, (outs), (ins), "NOP", []>;

def EXIT    : SimInst<0x01, (outs), (ins), "EXIT", []>;

def NEG    : SimInst<0x20, (outs GPR:$r1), (ins GPR:$r2), "NEG\t$r1, $r2", []> {
  let r3_imm = 0;
}
def ADD     : ALU_rr<0x21, "ADD", [(set GPR:$r1, (add GPR:$r2, GPR:$r3))]>;
def SUB     : ALU_rr<0x22, "SUB", [(set GPR:$r1, (sub GPR:$r2, GPR:$r3))]>;
def MUL     : ALU_rr<0x23, "MUL", [(set GPR:$r1, (mul GPR:$r2, GPR:$r3))]>;
def DIV     : ALU_rr<0x24, "DIV", [(set GPR:$r1, (sdiv GPR:$r2, GPR:$r3))]>;
// def DIV_REM : ALU_rr<0x25, "DIV_REM">;
// def POW     : ALU_rr<0x26, "POW">;
// def NOT     : SimInst<0x40, (outs GPR:$r1), (ins GPR:$r2),
//                       "NOT $r1, $r2", [(set GPR:$r1, (not GPR:$r2))]>;
def AND     : ALU_rr<0x41, "AND", [(set GPR:$r1, (and GPR:$r2, GPR:$r3))]>;
def OR      : ALU_rr<0x42, "OR", [(set GPR:$r1, (or GPR:$r2, GPR:$r3))]>;
def XOR     : ALU_rr<0x43, "XOR", [(set GPR:$r1, (xor GPR:$r2, GPR:$r3))]>;
// def SHL     : ALU_rr<0x44, "SHL", [(set GPR:$r1, (shl GPR:$r2, GPR:$r3))]>;
// def SHR     : ALU_rr<0x45, "SHR", [(set GPR:$r1, (shr GPR:$r2, GPR:$r3))]>;
// def SHRA    : ALU_rr<0x46, "SHRA", [(set GPR:$r1, (shra GPR:$r2, GPR:$r3))]>;

def ADDi     : ALU_ri<0x31, "ADDi", [(set GPR:$r1, (add GPR:$r2, SIMM16:$imm16))]>;
def SUBi     : ALU_ri<0x32, "SUBi", [(set GPR:$r1, (sub GPR:$r2, SIMM16:$imm16))]>;
def MULi     : ALU_ri<0x33, "MULi", [(set GPR:$r1, (mul GPR:$r2, SIMM16:$imm16))]>;
def DIVi     : ALU_ri<0x34, "DIVi", [(set GPR:$r1, (sdiv GPR:$r2, SIMM16:$imm16))]>;
// def DIV_REMi : ALU_ri<0x35, "DIV_REMi", [(set GPR:$r1, (add GPR:$r2, SIMM16:$imm16))]>;
// def POWi     : ALU_ri<0x36, "POWi", [(set GPR:$r1, (add GPR:$r2, SIMM16:$imm16))]>;
// def POWi_    : ALU_ri<0x37, "POWi_", [(set GPR:$r1, (add GPR:$r2, SIMM16:$imm16))]>;
def ANDi     : ALU_ri<0x51, "ANDi", [(set GPR:$r1, (and GPR:$r2, SIMM16:$imm16))]>;
def ORi      : ALU_ri<0x52, "ORi", [(set GPR:$r1, (or GPR:$r2, SIMM16:$imm16))]>;
def XORi     : ALU_ri<0x53, "XORi", [(set GPR:$r1, (xor GPR:$r2, SIMM16:$imm16))]>;
// def SHLi     : ALU_ri<0x54, "SHLi", [(set GPR:$r1, (shl GPR:$r2, SIMM16:$imm16))]>;
// def SHRi     : ALU_ri<0x55, "SHRi", [(set GPR:$r1, (shr GPR:$r2, SIMM16:$imm16))]>;
// def SHRAi    : ALU_ri<0x56, "SHRAi", [(set GPR:$r1, (shra GPR:$r2, SIMM16:$imm16))]>;


def MOV      : SimInst<0x10, (outs GPR:$r1), (ins GPR:$r2), "MOV\t$r1, $r2", []> {
  let r3_imm = 0;
}
def MOVli    : SimInst<0x11, (outs GPR:$r1), (ins i32li16u:$imm16),
                       "MOVli\t$r1, $imm16", [(set GPR:$r1, i32li16u:$imm16)]> {
  let r2 = 0;
}
def MOVhi    : SimInst<0x12, (outs GPR:$r1), (ins i32li16u:$imm16),
                       "MOVhi\t$r1, $imm16", [(set GPR:$r1, i32li16u:$imm16)]> {
  let r2 = 0;
}


// def B : Instruction {
//   let isTerminator = 1;
//   let isBranch = 1;
//   let isBarrier = 1;
//   let Namespace = "SIM";
//   let DecoderNamespace = "SIM";
//   let Size = 4;

//   field bits<32> Inst;

//   bits<16> r3_imm;

//   let Inst{31-24} = 0x02;
//   let Inst{23-20} = 0;
//   let Inst{19-16} = 0;
//   let Inst{15-0}  = r3_imm;

//   let OutOperandList = (outs);
//   let InOperandList  = (ins UIMM16:$r3_imm);
//   let AsmString   = "B\t$r3_imm";
//   let Pattern     = [(br bb:$r3_imm)];

//   let Itinerary = NoItinerary;
// }

let isTerminator = 1, isBranch = 1, isBarrier = 1 in {
def B        : SimInst<0x02, (outs), (ins BTARGET16:$r3_imm), "B\t$r3_imm", [(br bb:$r3_imm)]> {
  let r1 = 0;
  let r2 = 0;
}
def BR       : SimInstBranch<0x05, (outs), (ins GPR:$r1), "BR", [(brind GPR:$r1)]>;
def BEQ      : BCOND<0x06, "BEQ">;
def BNE      : BCOND<0x07, "BNE">;
def BGT      : BCOND<0x08, "BGT">;
def BLE      : BCOND<0x09, "BLE">;
}

// def IsOrAdd: PatFrag<(ops node:$A, node:$B), (or node:$A, node:$B), [{
//   return isOrEquivalentToAdd(N);
// }]>;


// def LDI: SimInst<0x0, (outs GPR:$rd), (ins GPR:$rs1, SIMM16:$imm),
//               "ldi\t$rd, ${imm}(${rs1})",
//               []>;
// def : Pat<(i32 (load ADDRrr:$rs1)), (LDI ADDRrr:$rs1, 0)>;
// def : Pat<(i32 (load (add ADDRrr:$rs1, SIMM16:$imm))),
//           (LDI ADDRrr:$rs1, SIMM16:$imm)>;
// def : Pat<(i32 (load (IsOrAdd ADDRri:$rs1, SIMM16:$imm))),
//           (LDI ADDRri:$rs1, SIMM16:$imm)>;

def LD : Instruction {
  let Namespace = "SIM";
  let DecoderNamespace = "SIM";
  let hasSideEffects = 0;
  let mayLoad = 1;
  let mayStore = 0;
  let Size = 4;

  field bits<32> Inst;

  // bits<8> Opcode = 0x70;
  bits<4>  r1;
  bits<4>  r2;
  bits<16> r3_imm;

  let Inst{31-24} = 0x70;
  let Inst{23-20} = r1;
  let Inst{19-16} = r2;
  let Inst{15-0}  = r3_imm;

  let OutOperandList = (outs GPR:$r1);
  let InOperandList  = (ins GPR:$r2, SIMM16:$r3_imm);
  let AsmString   = "LD\t$r1, $r2, $r3_imm";
  let Pattern     = [(set GPR:$r1, (load (add ADDRrr:$r2, SIMM16:$r3_imm)))];

  let Itinerary = NoItinerary;
}

def LDi : Instruction {
  let Namespace = "SIM";
  let DecoderNamespace = "SIM";
  let hasSideEffects = 0;
  let mayLoad = 1;
  let mayStore = 0;
  let Size = 4;

  field bits<32> Inst;

  // bits<8> Opcode = 0x70;
  bits<4>  r1;
  bits<4>  r2;
  bits<16> r3_imm;

  let Inst{31-24} = 0x72;
  let Inst{23-20} = r1;
  let Inst{19-16} = r2;
  let Inst{15-0}  = r3_imm;

  let OutOperandList = (outs GPR:$r1);
  let InOperandList  = (ins GPR:$r2, SIMM16:$r3_imm);
  let AsmString   = "LDi\t$r1, $r2, $r3_imm";
  let Pattern     = [(set GPR:$r1, (load (add ADDRri:$r2, SIMM16:$r3_imm)))];

  let Itinerary = NoItinerary;
}

let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in
def LDI_: SimInst<0x0, (outs GPR:$r1), (ins GPR:$r2, SIMM16:$r3_imm),
                  "LDi\t$r1, ${r3_imm}(${r2})",
                  []>;
def : Pat<(i32 (load ADDRrr:$r1)), (LDI_ ADDRrr:$r1, 0)>;

def ST : Instruction {
  let Namespace = "SIM";
  let DecoderNamespace = "SIM";
  let hasSideEffects = 0;
  let mayLoad = 0;
  let mayStore = 1;
  let Size = 4;

  field bits<32> Inst;

  // bits<8> Opcode = 0x70;
  bits<4>  r1;
  bits<4>  r2;
  bits<16> r3_imm;

  let Inst{31-24} = 0x71;
  let Inst{23-20} = r1;
  let Inst{19-16} = r2;
  let Inst{15-0}  = r3_imm;

  let OutOperandList = (outs GPR:$r1);
  let InOperandList  = (ins GPR:$r2, GPR:$r3_imm);
  let AsmString   = "ST\t$r1, $r2, $r3_imm";
  let Pattern     = [(store GPR:$r1, (add ADDRrr:$r2, GPR:$r3_imm))];

  let Itinerary = NoItinerary;
}
// [(store i32:$val, addr:$addr)]
def STi : Instruction {
  let Namespace = "SIM";
  let DecoderNamespace = "SIM";
  let hasSideEffects = 0;
  let mayLoad = 1;
  let mayStore = 0;
  let Size = 4;

  field bits<32> Inst;

  // bits<8> Opcode = 0x70;
  bits<4>  r1;
  bits<4>  r2;
  bits<16> r3_imm;

  let Inst{31-24} = 0x73;
  let Inst{23-20} = r1;
  let Inst{19-16} = r2;
  let Inst{15-0}  = r3_imm;

  let OutOperandList = (outs GPR:$r1);
  let InOperandList  = (ins GPR:$r2, SIMM16:$r3_imm);
  let AsmString   = "STi\t$r1, $r2, $r3_imm";
  let Pattern     = [(store GPR:$r1, (add ADDRri:$r2, SIMM16:$r3_imm))];

  let Itinerary = NoItinerary;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in
def STI_: SimInst<0x0, (outs), (ins GPR:$r1, GPR:$r2, SIMM16:$r3_imm),
                  "STi\t$r1, ${r3_imm}(${r2})",
                  []>;
def : Pat<(store (i32 GPR:$r1), ADDRrr:$r2),
          (STI_ GPR:$r1, ADDRrr:$r2, 0)>;

// let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
// def LD       : SimInst<0x70,
//                        (outs GPR:$r1), (ins GPR:$r2, SIMM16:$imm16),
//                        "LD\t$r1, ${imm16}(${r2})",
//                        [(i32 (load (add ADDRrr:$r2, SIMM16:$imm16)))]>;
// def LDi      : SimInst<0x72,
//                        (outs GPR:$r1), (ins GPR:$r2, SIMM16:$imm16),
//                        "LDi\t$r1, ${imm16}(${r2})",
//                        [(i32 (load (IsOrAdd ADDRri:$r2, SIMM16:$imm16)))]>;
// }
// "LD\t$r1, $r2, $imm16)"
// "LDi\t$r1, $r2, $imm16)"

// let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
// def ST       : SimInst<0x71,
//                        (outs), (ins GPR:$r1, GPR:$r2, SIMM16:$imm16),
//                        "ST\t$r1, $r2, $imm16",
//                        [(store (i32 GPR:$r1), (add ADDRrr:$r1, SIMM16:$imm16))]>;
// def STi      : SimInst<0x73,
//                        (outs), (ins GPR:$r1, GPR:$r2, SIMM16:$imm16),
//                        "STi\t$r1, $r2, $imm16",
//                        [(store (i32 GPR:$r1), (add ADDRri:$r1, SIMM16:$imm16))]>;
// }

//===----------------------------------------------------------------------===//
// Pseudo Instructions
//===----------------------------------------------------------------------===//

let Defs = [R2], Uses = [R2] in {
def ADJCALLSTACKDOWN : SimPseudoInst<(outs), (ins i32imm:$amt, i32imm:$amt2),
                               "# ADJCALLSTACKDOWN $amt, $amt2",
                               [(callseq_start timm:$amt, timm:$amt2)]>;
def ADJCALLSTACKUP : SimPseudoInst<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                            "# ADJCALLSTACKUP $amt1",
                            [(callseq_end timm:$amt1, timm:$amt2)]>;
}

// TODO: redefine RA from R0 to smth else?
def PseudoRET : SimPseudoInst<(outs), (ins), "# PseudoRET",
                [(SimRet)]>,
                PseudoInstExpansion<(BR R0)> {
  let isBarrier = 1;
  let isReturn = 1;
  let isTerminator = 1;
}
