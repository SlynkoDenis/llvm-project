//===-- Sim.td - Describe the Sim Target Machine ----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This is the top level entry point for the Sim target.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Target-dependent interfaces
//===----------------------------------------------------------------------===//

include "SimRegisterInfo.td"

//===----------------------------------------------------------------------===//
// Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "SimCallingConv.td"
include "SimInstrInfo.td"

def SimInstrInfo : InstrInfo;

def SimAsmParser : AsmParser {
  bit ShouldEmitMatchRegisterName = 0;
}
// TODO: decide if SimAsmParserVariant is required
// def SimAsmParserVariant : AsmParserVariant {
//   let RegisterPrefix = "%";
// }

def : Processor<"Sim", NoItineraries, []>;

def SimAsmWriter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  int PassSubtarget = 1;
  int Variant = 0;
}

//===----------------------------------------------------------------------===//
// Sim processors supported.
//===----------------------------------------------------------------------===//

// Will generate SimGenAsmWrite.inc included by SimInstPrinter.cpp, contents
//  as follows,
// void SimInstPrinter::printInstruction(const MCInst *MI, raw_ostream &O) {...}
// const char *SimInstPrinter::getRegisterName(unsigned RegNo) {...}
def Sim : Target {
  let InstructionSet = SimInstrInfo;
  // let AssemblyParsers  = [SimAsmParser];
  // let AssemblyParserVariants  = [SimAsmParserVariant];
  // let AssemblyWriters = [SimAsmWriter];
  let AllowRegisterRenaming = 1;
}
