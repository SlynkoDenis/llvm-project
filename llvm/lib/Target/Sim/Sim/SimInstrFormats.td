//===-- SimInstrFormats.td - Sim Instruction Formats -----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Describe Sim instructions format
//
//  CPU INSTRUCTION FORMATS
//
//  FF FF FF FF
//        ^^ ^^ - r3_imm - third reg or imm
//      ^       - r2     - second reg
//     ^        - r1     - first reg
//  ^^          - opcode - operation code
//
//===----------------------------------------------------------------------===//

// Generic Sim Format
class SimInst<bits<8> op, dag outs, dag ins, string asmstr, list<dag> pattern>: Instruction
{
  // Inst and Size: for tablegen(... -gen-emitter) and
  // tablegen(... -gen-disassembler) in CMakeLists.txt
  let Namespace = "Sim";
  let DecoderNamespace = "Sim";
  let Size = 4;

  field bits<32> Inst;

  bits<8> Opcode = op;
  bits<4>  r1 = 0;
  bits<4>  r2 = 0;
  bits<16> r3_imm = 0;

  let Inst{31-24} = Opcode;
  let Inst{23-20} = r1;
  let Inst{19-16} = r2;
  let Inst{15-0}  = r3_imm;

  let OutOperandList = outs;
  let InOperandList  = ins;
  let AsmString   = asmstr;
  let Pattern     = pattern;
}


class SimInstrRegister<bits<8> opcode, dag outs,
                       dag ins, string asmstr> : SimInst<opcode, outs, ins, asmstr, []> {
  bits<4> r3;

  let Inst{15-12} = r3;
  let Inst{11-0}  = 0;
}


class ALU_rr<bits<8> opcode, string asmstr> : SimInst<opcode, (outs GPR:$rd), (ins GPR:$r1, GPR:$r2),
                                                      asmstr # "\t" # "$rd, $r1, $r2", []>;


class ALU_ri<bits<8> opcode, string asmstr> : SimInst<opcode, (outs GPR:$rd), (ins GPR:$r1, GPR:$imm12),
                                                      asmstr # "\t" # "$rd, $r1, $imm12", []>;
