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
    RET,
    CALL,
    BR_CC,
    SELECT_CC,
  };
}

class SimTargetLowering : public TargetLowering {
    const SimSubtarget *Subtarget;
public:
    SimTargetLowering(const TargetMachine &TM, const SimSubtarget &STI);

    MachineBasicBlock *EmitInstrWithCustomInserter(MachineInstr &MI,
                                                   MachineBasicBlock *BB) const override;

    SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

    const char *getTargetNodeName(unsigned Opcode) const override;

    Register getRegisterByName(const char* RegName, LLT VT,
                               const MachineFunction &MF) const override;

    /// getSetCCResultType - Return the ISD::SETCC ValueType
    EVT getSetCCResultType(const DataLayout &DL, LLVMContext &Context,
                           EVT VT) const override;

    /// Return true if the addressing mode represented by AM is legal for this
    /// target, for a load/store of the specified type.
    bool isLegalAddressingMode(const DataLayout &DL, const AddrMode &AM, Type *Ty,
                               unsigned AS,
                               Instruction *I = nullptr) const override;

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

    bool CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
                        bool IsVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &ArgsFlags,
                        LLVMContext &Context) const override;

    SDValue PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const override;

    bool mayBeEmittedAsTailCall(const CallInst *CI) const override {
      return false;
    }

    void ReplaceNodeResults(SDNode *N,
                            SmallVectorImpl<SDValue>& Results,
                            SelectionDAG &DAG) const override {
      llvm_unreachable("TBD");
    }

private:
    MachineBasicBlock *expandSelectCC(MachineInstr &MI, MachineBasicBlock *BB) const;
};
} // end namespace llvm

#endif    // LLVM_LIB_TARGET_SIM_SIMISELLOWERING_H
