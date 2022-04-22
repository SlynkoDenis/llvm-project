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
  CCIfType<[i8, i16], CCPromoteToType<i32>>,
  // i32 f32 arguments get passed in integer registers if there is space.
  CCIfType<[i32, f32, i64], CCAssignToReg<[R10, R11, R12, R13, R14, R15]>>,

  // Alternatively, they are assigned to the stack in 4-byte aligned units.
  CCIfType<[i32], CCAssignToStack<4, 4>>,
  CCIfType<[i64], CCAssignToStack<8, 4>>,
  CCIfType<[f32], CCAssignToStack<4, 4>>
]>;


def RetCC_Sim : CallingConv<[
  CCIfType<[i32, f32, i64], CCAssignToReg<[R10, R11, R12, R13, R14, R15]>>,

  // Alternatively, they are assigned to the stack in 4-byte aligned units.
  CCIfType<[i32], CCAssignToStack<4, 4>>,
  CCIfType<[i64], CCAssignToStack<8, 4>>,
  CCIfType<[f32], CCAssignToStack<4, 4>>
]>;

def CSR_Sim : CalleeSavedRegs<(add R2, R3, R5, R6, R7, R8)>;
