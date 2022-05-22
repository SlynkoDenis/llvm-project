//===-- SimRegisterInfo.td - Sim Register defs -----------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the Sim register file
//===----------------------------------------------------------------------===//

// For tablegen(... -gen-emitter)  in CMakeLists.txt
class SimReg<bits<16> Enc, string n> : Register<n> {
  let HWEncoding = Enc;
  let Namespace = "SIM";
}

// Sim CPU Registers
class SimGPRReg<bits<16> Enc, string n> : SimReg<Enc, n> {
  let Namespace = "SIM";
}

//===----------------------------------------------------------------------===//
//@Registers
//===----------------------------------------------------------------------===//
//@ All registers definition
foreach I = 0-15 in {
  def R#I    : SimGPRReg<I, "r"#I>, DwarfRegNum<[I]>;
}

//===----------------------------------------------------------------------===//
//@Register Classes
//===----------------------------------------------------------------------===//

def CPURegs : RegisterClass<"SIM", [i32], 32, (add
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)>;

def GPR : RegisterClass<"SIM", [i32], 32, (add CPURegs)>;

// TODO: decide if GPROpnd is required
def GPROpnd : RegisterOperand<GPR>;
