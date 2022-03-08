//===-- SimCallingConv.td - Calling Conventions Sim ----*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This describes the calling conventions for the Sim 32-bit architecture.
//
//===----------------------------------------------------------------------===//

def CC_Sim : CallingConv<[
  // Custom assign SRet to [sp+64].
  // TODO: decide one of the options
  // CCIfSRet<CCCustom<"CC_Sparc_Assign_SRet">>,
  // TODO: choose reg
  CCIfSRet<CCIfType<[i32], CCAssignToReg<[R14]>>>,
  // i32 f32 arguments get passed in integer registers if there is space.
  CCIfType<[i32, f32], CCAssignToReg<[R10, R11, R12, R13, R14, R15]>>,

  // Alternatively, they are assigned to the stack in 4-byte aligned units.
  CCAssignToStack<4, 4>
]>;

def RetCC_Sim : CallingConv<[
  // TODO: choose regs
  CCIfType<[i32], CCAssignToReg<[R2]>>,
  CCIfType<[f32], CCAssignToReg<[R2]>>
]>;

// TODO: decide if it's needed
def CSR_Sim : CalleeSavedRegs<(add (sequence "R%d", 0, 1), R15)>;
