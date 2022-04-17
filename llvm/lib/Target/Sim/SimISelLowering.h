//===-- SimISelLowering.h - Sim DAG Lowering Interface ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that Sim uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SIM_SIMISELLOWERING_H
#define LLVM_LIB_TARGET_SIM_SIMISELLOWERING_H

#include "Sim.h"
#include "llvm/CodeGen/TargetLowering.h"

namespace llvm {

class SimSubtarget;
class SimTargetMachine;

namespace SIMISD {
  enum NodeType : unsigned {
    FIRST_NUMBER = ISD::BUILTIN_OP_END,
    CMPICC,      // Compare two GPR operands, set icc+xcc.
    CMPFCC,      // Compare two FP operands, set fcc.
    BRICC,       // Branch to dest on icc condition
    BRXCC,       // Branch to dest on xcc condition (64-bit only).
    BRFCC,       // Branch to dest on fcc condition
    SELECT_ICC,  // Select between two values using the current ICC flags.
    SELECT_XCC,  // Select between two values using the current XCC flags.
    SELECT_FCC,  // Select between two values using the current FCC flags.
    Hi, Lo,      // Hi/Lo operations, typically on a global address.
    FTOI,        // FP to Int within a FP register.
    ITOF,        // Int to FP within a FP register.
    FTOX,        // FP to Int64 within a FP register.
    XTOF,        // Int64 to FP within a FP register.
    CALL,        // A call instruction.
    RET,         // Return with a flag operand.
    GLOBAL_BASE_REG, // Global base reg for PIC.
    FLUSHW,      // FLUSH register windows to stack.
  };
}

class SimTargetLowering : public TargetLowering {
    const SimSubtarget *Subtarget;
public:
    SimTargetLowering(const TargetMachine &TM, const SimSubtarget &STI);
    SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

    const char *getTargetNodeName(unsigned Opcode) const override;

    Register getRegisterByName(const char* RegName, LLT VT,
                               const MachineFunction &MF) const override;

    /// getSetCCResultType - Return the ISD::SETCC ValueType
    EVT getSetCCResultType(const DataLayout &DL, LLVMContext &Context,
                           EVT VT) const override;

    SDValue
    LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                         const SmallVectorImpl<ISD::InputArg> &Ins,
                         const SDLoc &dl, SelectionDAG &DAG,
                         SmallVectorImpl<SDValue> &InVals) const override;

    SDValue LowerCall(TargetLowering::CallLoweringInfo &CLI,
                      SmallVectorImpl<SDValue> &InVals) const override;

    SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &Outs,
                        const SmallVectorImpl<SDValue> &OutVals,
                        const SDLoc &dl, SelectionDAG &DAG) const override;

    // SDValue withTargetFlags(SDValue Op, unsigned TF, SelectionDAG &DAG) const;
    // SDValue makeHiLoPair(SDValue Op, unsigned HiTF, unsigned LoTF,
    //                      SelectionDAG &DAG) const;
    // SDValue makeAddress(SDValue Op, SelectionDAG &DAG) const;

    // SDValue LowerF128Op(SDValue Op, SelectionDAG &DAG,
    //                     const char *LibFuncName,
    //                     unsigned numArgs) const;

    // SDValue PerformBITCASTCombine(SDNode *N, DAGCombinerInfo &DCI) const;

    // SDValue bitcastConstantFPToInt(ConstantFPSDNode *C, const SDLoc &DL,
    //                                SelectionDAG &DAG) const;

    SDValue PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const override;

    void ReplaceNodeResults(SDNode *N,
                            SmallVectorImpl<SDValue>& Results,
                            SelectionDAG &DAG) const override;
};
} // end namespace llvm

#endif    // LLVM_LIB_TARGET_SIM_SIMISELLOWERING_H
