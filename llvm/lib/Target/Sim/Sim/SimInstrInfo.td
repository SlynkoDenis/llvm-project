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
def SimRet : SDNode<"SimISD::Ret", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

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
let isBranch=1, isTerminator=1, isBarrier=1, hasDelaySlot = 1,
    isIndirectBranch = 1 in
class Branches<bits<8> op, string instr_asm, RegisterClass RC>:
  SimInst<op, (outs), (ins RC:$r1),
     !strconcat(instr_asm, "\t$r1"), [(brind RC:$r1)]> {
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
