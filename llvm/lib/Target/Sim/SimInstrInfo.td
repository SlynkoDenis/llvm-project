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

def SDT_SimRet          : SDTypeProfile<0, 1, [SDTCisInt<0>]>;

// Return
def SimRet : SDNode<"SimISD::RET", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

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

//===----------------------------------------------------------------------===//
// Instructions specific format
//===----------------------------------------------------------------------===//

//@Branches {
// let isBranch = 1, isTerminator = 1, isBarrier = 1, hasDelaySlot = 1, isIndirectBranch = 1 in {
// class Branches<bits<8> op, string instr_asm, RegisterClass RC>
//     : SimInst<op, (outs), (ins RC:$r1),
//               !strconcat(instr_asm, "\t$r1"), [(brind RC:$r1)]>;
// }
class Branches<bits<8> op, string instr_asm, RegisterClass RC>
    : SimInst<op, (outs), (ins RC:$r1),
              !strconcat(instr_asm, "\t$r1"), [(brind RC:$r1)]> {
  let isBranch = 1;
  let isTerminator = 1;
  let isBarrier = 1;
  let hasDelaySlot = 1;
  let isIndirectBranch = 1;
}

//@Branches }

// Return instruction
class RetBase<RegisterClass RC>: Branches<0x3c, "RET", RC> {
  let isReturn = 1;
  let isCodeGenOnly = 1;
  let hasCtrlDep = 1;
  let hasExtraSrcRegAllocReq = 1;
}


//===----------------------------------------------------------------------===//
// Instruction definition
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Sim Instructions
//===----------------------------------------------------------------------===//

def RET     : RetBase<CPURegs>;

/// No operation
def NOP     : SimInst<0, (outs), (ins), "nop", []>;

def EXIT    : SimInst<0x01, (outs), (ins), "EXIT", []>;

def NEG    : SimInst<0x20, (outs GPR:$r1), (ins GPR:$r2), "NEG\t$r1, $r2", []> {
  let r3_imm = 0;
}
def ADD     : ALU_rr<0x21, "ADD">;
def SUB     : ALU_rr<0x22, "SUB">;
def MUL     : ALU_rr<0x23, "MUL">;
def DIV     : ALU_rr<0x24, "DIV">;
def DIV_REM : ALU_rr<0x25, "DIV_REM">;
def POW     : ALU_rr<0x26, "POW">;
def NOT     : SimInst<0x40, (outs GPR:$r1), (ins GPR:$r2),
                      "NOT $r1, $r2", []>;
def AND     : ALU_rr<0x41, "AND">;
def OR      : ALU_rr<0x42, "OR">;
def XOR     : ALU_rr<0x43, "XOR">;
def SHL     : ALU_rr<0x44, "SHL">;
def SHR     : ALU_rr<0x45, "SHR">;
def SHRA    : ALU_rr<0x46, "SHRA">;

def ADDi     : ALU_ri<0x31, "ADDi">;
def SUBi     : ALU_ri<0x32, "SUBi">;
def MULi     : ALU_ri<0x33, "MULi">;
def DIVi     : ALU_ri<0x34, "DIVi">;
def DIV_REMi : ALU_ri<0x35, "DIV_REMi">;
def POWi     : ALU_ri<0x36, "POWi">;
def POWi_    : ALU_ri<0x37, "POWi_">;
def ANDi     : ALU_ri<0x51, "ANDi">;
def ORi      : ALU_ri<0x52, "ORi">;
def XORi     : ALU_ri<0x53, "XORi">;
def SHLi     : ALU_ri<0x54, "SHLi">;
def SHRi     : ALU_ri<0x55, "SHRi">;
def SHRAi    : ALU_ri<0x56, "SHRAi">;

def MOV      : SimInst<0x10, (outs GPR:$r1), (ins GPR:$r2), "MOV\t$r1, $r2", []> {
  let r3_imm = 0;
}
def MOVli    : SimInst<0x11, (outs GPR:$r1), (ins GPR:$imm16), "MOVli\t$r1, $imm16", []> {
  let r2 = 0;
}
def MOVhi    : SimInst<0x12, (outs GPR:$r1), (ins GPR:$imm16), "MOVhi\t$r1, $imm16", []> {
  let r2 = 0;
}


def B        : SimInst<0x02, (outs), (ins GPR:$imm16), "B\t$imm16", []>;
def BEQ      : BCOND<0x06, "BEQ">;
def BNE      : BCOND<0x07, "BNE">;
def BGT      : BCOND<0x08, "BGT">;
def BLE      : BCOND<0x09, "BLE">;
def BR       : SimInstBranch<0x05, (outs), (ins GPR:$r1), "BR">;


// def LD       : SimInst<0x70, (outs GPR:$r1), (ins GPR:$r2, GPR:$imm16), "LD\t$r1, $r2, $imm16", []>;
def LD       : ALU_ri<0x70, "LD">;
def LDi      : ALU_ri<0x72, "LDi">;

def ST       : STORE<0x71, "ST">;
def STi      : STORE<0x73, "STi">;

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
