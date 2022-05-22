//===-- SimISelLowering.cpp - Sim DAG Lowering Implementation ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the interfaces that Sim uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#include "SimISelLowering.h"
#include "SimMachineFunctionInfo.h"
#include "SimRegisterInfo.h"
#include "SimTargetMachine.h"
#include "SimTargetObjectFile.h"
#include "MCTargetDesc/SimInfo.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"

using namespace llvm;

#define DEBUG_TYPE "Sim-lower"

#include "SimGenCallingConv.inc"
#include "SimGenRegisterInfo.inc"


static SDValue convertValVTToLocVT(SelectionDAG &DAG, SDValue Val,
                                   const CCValAssign &VA, const SDLoc &DL) {
  EVT LocVT = VA.getLocVT();

  if (VA.getValVT() == MVT::f32) {
    llvm_unreachable("TBD");
  }

  // TODO: test this function
  // Promote the value if needed.
  switch (VA.getLocInfo()) {
  default:
    llvm_unreachable("Unexpected LocInfo");
  case CCValAssign::Full:
    break;
  case CCValAssign::SExt:
    Val = DAG.getNode(ISD::SIGN_EXTEND, DL, LocVT, Val);
    break;
  case CCValAssign::ZExt:
    Val = DAG.getNode(ISD::ZERO_EXTEND, DL, LocVT, Val);
    break;
  case CCValAssign::BCvt:
    Val = DAG.getNode(ISD::BITCAST, DL, LocVT, Val);
    break;
  }
  return Val;
}

SDValue
SimTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                               bool IsVarArg,
                               const SmallVectorImpl<ISD::OutputArg> &Outs,
                               const SmallVectorImpl<SDValue> &OutVals,
                               const SDLoc &DL, SelectionDAG &DAG) const {
  assert(IsVarArg == false && "TBD");
  MachineFunction &MF = DAG.getMachineFunction();

  // CCValAssign - represent the assignment of the return value to locations.
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs,
                 *DAG.getContext());

  // Analyze return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_Sim);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0, end = RVLocs.size(); i < end; ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    SDValue Arg = convertValVTToLocVT(DAG, OutVals[i], VA, DL);

    assert(!VA.needsCustom() && "do not support custom assignments");
    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Arg, Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    // TODO: this action is redundant for Simulation model - we don't need Glue
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  if (MF.getFunction().hasStructRetAttr()) {
    llvm_unreachable("TBD");
  }

  RetOps[0] = Chain;  // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode()) {
    RetOps.push_back(Flag);
  }

  return DAG.getNode(SIMISD::RET, DL, MVT::Other, RetOps);
}


bool SimTargetLowering::CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
                                       bool IsVarArg,
                                       const SmallVectorImpl<ISD::OutputArg> &Outs,
                                       LLVMContext &Context) const {
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, Context);
  if (!CCInfo.CheckReturn(Outs, RetCC_Sim)) {
    return false;
  }
  if (CCInfo.getNextStackOffset() != 0 && IsVarArg) {
    llvm_unreachable("TBD");
  }
  return true;
}

SDValue SimTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  // SimMachineFunctionInfo *FuncInfo = MF.getInfo<SimMachineFunctionInfo>();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_Sim);

  unsigned InIdx = 0;
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i, ++InIdx) {
    CCValAssign &VA = ArgLocs[i];

    if (VA.isRegLoc()) {
      EVT LocVT = VA.getLocVT();
      const TargetRegisterClass *RC = getRegClassFor(LocVT.getSimpleVT());
      Register VReg = RegInfo.createVirtualRegister(RC);
      MF.getRegInfo().addLiveIn(VA.getLocReg(), VReg);
      SDValue Arg = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i32);
      if (LocVT == MVT::f32) {
        Arg = DAG.getNode(ISD::BITCAST, dl, MVT::f32, Arg);
      } else if (LocVT != MVT::i32) {
        Arg = DAG.getNode(ISD::AssertSext, dl, MVT::i32, Arg,
                          DAG.getValueType(LocVT));
        Arg = DAG.getNode(ISD::TRUNCATE, dl, LocVT, Arg);
      }
      InVals.push_back(Arg);
    } else if (VA.isMemLoc()) {
      unsigned Offset = VA.getLocMemOffset();
      auto PtrVT = getPointerTy(DAG.getDataLayout());

      int FI = MF.getFrameInfo().CreateFixedObject(4,
                                                  Offset,
                                                  true);
      SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
      SDValue Load;
      if (VA.getValVT() == MVT::i32 || VA.getValVT() == MVT::f32) {
        Load = DAG.getLoad(VA.getValVT(), dl, Chain, FIPtr, MachinePointerInfo());
      } else {
        // We shouldn't see any other value types here.
        llvm_unreachable("Unexpected ValVT encountered in frame lowering.");
      }
      InVals.push_back(Load);
    } else {
      llvm_unreachable("");
    }
  }

  if (MF.getFunction().hasStructRetAttr()) {
    llvm_unreachable("TBD: LowerFormalArguments for hasStructRetAttr Function");
  }

  // Store remaining ArgRegs to the stack if this is a varargs function.
  if (isVarArg) {
    llvm_unreachable("TBD: LowerFormalArguments for isVarArg");
    // TODO: change regs?
    // static const MCPhysReg ArgRegs[] = {
    //   SIM::R10, SIM::R11, SIM::R12, SIM::R13, SIM::R14, SIM::R15
    // };
    // unsigned NumAllocated = CCInfo.getFirstUnallocated(ArgRegs);
    // const MCPhysReg *CurArgReg = ArgRegs + NumAllocated;
    // const MCPhysReg *ArgRegEnd = ArgRegs + 6;
    // unsigned ArgOffset = CCInfo.getNextStackOffset();
    // if (NumAllocated == 6) {
    //   llvm_unreachable("");
    //   ArgOffset += 0;
    // } else {
    //   assert(!ArgOffset);
    //   ArgOffset = 68 + 4 * NumAllocated;
    // }

    // // Remember the vararg offset for the va_start implementation.
    // FuncInfo->setVarArgsFrameOffset(ArgOffset);

    // std::vector<SDValue> OutChains;

    // for (; CurArgReg != ArgRegEnd; ++CurArgReg) {
    //   Register VReg = RegInfo.createVirtualRegister(&SIM::GPRRegClass);
    //   MF.getRegInfo().addLiveIn(*CurArgReg, VReg);
    //   SDValue Arg = DAG.getCopyFromReg(DAG.getRoot(), dl, VReg, MVT::i32);

    //   int FrameIdx = MF.getFrameInfo().CreateFixedObject(4, ArgOffset,
    //                                                      true);
    //   SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);

    //   OutChains.push_back(
    //       DAG.getStore(DAG.getRoot(), dl, Arg, FIPtr, MachinePointerInfo()));
    //   ArgOffset += 4;
    // }

    // if (!OutChains.empty()) {
    //   OutChains.push_back(Chain);
    //   Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains);
    // }
  }

  return Chain;
}

SDValue
SimTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                             SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG                     = CLI.DAG;
  SDLoc &dl                             = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals     = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins   = CLI.Ins;
  SDValue Chain                         = CLI.Chain;
  SDValue Callee                        = CLI.Callee;
  bool &isTailCall                      = CLI.IsTailCall;
  CallingConv::ID CallConv              = CLI.CallConv;
  bool isVarArg                         = CLI.IsVarArg;

  // Sim target does not yet support tail call optimization.
  // TODO: implement it
  isTailCall = false;

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_Sim);

  // Get the size of the outgoing arguments stack space requirement.
  unsigned ArgsSize = CCInfo.getNextStackOffset();

  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();

  // taken from RISCV
  // Create local copies for byval args.
  SmallVector<SDValue, 8> ByValArgs;
  for (unsigned i = 0, e = Outs.size(); i != e; ++i) {
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    if (!Flags.isByVal()) {
      continue;
    }

    SDValue Arg = OutVals[i];
    unsigned Size = Flags.getByValSize();
    Align Alignment = Flags.getNonZeroByValAlign();

    if (Size > 0U) {
      int FI = MFI.CreateStackObject(Size, Alignment, false);
      SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
      SDValue SizeNode = DAG.getConstant(Size, dl, MVT::i32);

      Chain = DAG.getMemcpy(Chain, dl, FIPtr, Arg, SizeNode, Alignment,
                            false,             // isVolatile,
                            (Size <= 32),      // AlwaysInline if size <= 32,
                            isTailCall,        // isTailCall
                            MachinePointerInfo(), MachinePointerInfo());
      ByValArgs.push_back(FIPtr);
    } else {
      SDValue nullVal;
      ByValArgs.push_back(nullVal);
    }
  }

  Chain = DAG.getCALLSEQ_START(Chain, ArgsSize, 0, dl);

  SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;
  // SDValue StackPtr;
  for (unsigned i = 0, j = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    SDValue ArgValue = OutVals[i];
    ISD::ArgFlagsTy Flags = Outs[i].Flags;

    if (VA.needsCustom()) {
      llvm_unreachable("TBD - needsCustom LowerCall");
    }

    // Promote the value if needed.
    // For now, only handle fully promoted and indirect arguments.
    if (VA.getLocInfo() == CCValAssign::Indirect) {
      llvm_unreachable("unsupported Indirect calls");
      // Store the argument in a stack slot and pass its address.
    //   Align StackAlign =
    //       std::max(getPrefTypeAlign(Outs[i].ArgVT, DAG),
    //                getPrefTypeAlign(ArgValue.getValueType(), DAG));
    //   TypeSize StoredSize = ArgValue.getValueType().getStoreSize();
    //   // If the original argument was split (e.g. i128), we need
    //   // to store the required parts of it here (and pass just one address).
    //   // Vectors may be partly split to registers and partly to the stack, in
    //   // which case the base address is partly offset and subsequent stores are
    //   // relative to that.
    //   unsigned ArgIndex = Outs[i].OrigArgIndex;
    //   unsigned ArgPartOffset = Outs[i].PartOffset;
    //   assert(VA.getValVT().isVector() || ArgPartOffset == 0);
    //   // Calculate the total size to store. We don't have access to what we're
    //   // actually storing other than performing the loop and collecting the
    //   // info.
    //   SmallVector<std::pair<SDValue, SDValue>> Parts;
    //   while (i + 1 != e && Outs[i + 1].OrigArgIndex == ArgIndex) {
    //     SDValue PartValue = OutVals[i + 1];
    //     unsigned PartOffset = Outs[i + 1].PartOffset - ArgPartOffset;
    //     SDValue Offset = DAG.getIntPtrConstant(PartOffset, DL);
    //     EVT PartVT = PartValue.getValueType();
    //     if (PartVT.isScalableVector())
    //       Offset = DAG.getNode(ISD::VSCALE, DL, XLenVT, Offset);
    //     StoredSize += PartVT.getStoreSize();
    //     StackAlign = std::max(StackAlign, getPrefTypeAlign(PartVT, DAG));
    //     Parts.push_back(std::make_pair(PartValue, Offset));
    //     ++i;
    //   }
    //   SDValue SpillSlot = DAG.CreateStackTemporary(StoredSize, StackAlign);
    //   int FI = cast<FrameIndexSDNode>(SpillSlot)->getIndex();
    //   MemOpChains.push_back(
    //       DAG.getStore(Chain, DL, ArgValue, SpillSlot,
    //                    MachinePointerInfo::getFixedStack(MF, FI)));
    //   for (const auto &Part : Parts) {
    //     SDValue PartValue = Part.first;
    //     SDValue PartOffset = Part.second;
    //     SDValue Address =
    //         DAG.getNode(ISD::ADD, DL, PtrVT, SpillSlot, PartOffset);
    //     MemOpChains.push_back(
    //         DAG.getStore(Chain, DL, PartValue, Address,
    //                      MachinePointerInfo::getFixedStack(MF, FI)));
    //   }
    //   ArgValue = SpillSlot;
    } else {
      ArgValue = convertValVTToLocVT(DAG, ArgValue, VA, dl);
    }

    // Use local copy if it is a byval arg.
    if (Flags.isByVal()) {
      ArgValue = ByValArgs[j++];
    }

    if (VA.isRegLoc()) {
      // Queue up the argument copies and emit them at the end.
      if (VA.getLocVT() != MVT::f32) {
        RegsToPass.push_back(std::make_pair(VA.getLocReg(), ArgValue));
      }

      // TODO: implement without bitcast
      ArgValue = DAG.getNode(ISD::BITCAST, dl, MVT::i32, ArgValue);
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), ArgValue));
    } else {
      assert(VA.isMemLoc() && "Argument not register or memory");
      assert(!isTailCall && "Tail call not allowed if stack is used "
                            "for passing parameters");

      // Create a store off the stack pointer for this argument.
      SDValue StackPtr = DAG.getRegister(SIM::SP, MVT::i32);
      SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset(), dl);
      PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
      MemOpChains.push_back(
          DAG.getStore(Chain, dl, ArgValue, PtrOff, MachinePointerInfo()));
    }
  }

  // TODO: resolve StructRetAttr
  bool hasStructRetAttr = false;

  // Join the stores, which are independent of one another.
  // Make sure the occur before any copies into physregs.
  if (!MemOpChains.empty()) {
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOpChains);
  }

  SDValue Glue;
  // Build a sequence of copy-to-reg nodes, chained and glued together.
  for (auto &Reg : RegsToPass) {
    Chain = DAG.getCopyToReg(Chain, dl, Reg.first, Reg.second, Glue);
    Glue = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), dl, MVT::i32, 0);
  } else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i32);
  }

  // Returns a chain & a flag for retval copy to use
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);
  if (hasStructRetAttr) {
    llvm_unreachable("TBD");
    uint64_t SRetArgSize = 0;
    Ops.push_back(DAG.getTargetConstant(SRetArgSize, dl, MVT::i32));
  }
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Ops.push_back(DAG.getRegister(RegsToPass[i].first,
                                  RegsToPass[i].second.getValueType()));
  }

  // Add a register mask operand representing the call-preserved registers.
  const SimRegisterInfo *TRI = Subtarget->getRegisterInfo();
  const uint32_t *Mask =
      TRI->getRTCallPreservedMask(CallConv);
  assert(Mask && "Missing call preserved mask for calling convention");
  Ops.push_back(DAG.getRegisterMask(Mask));

  if (Glue.getNode()) {
    Ops.push_back(Glue);
  }

  Chain = DAG.getNode(SIMISD::CALL, dl, NodeTys, Ops);
  DAG.addNoMergeSiteInfo(Chain.getNode(), CLI.NoMerge);
  Glue = Chain.getValue(1);

  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(ArgsSize, dl, true),
                             DAG.getIntPtrConstant(0, dl, true), Glue, dl);
  Glue = Chain.getValue(1);

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RVInfo(CallConv, isVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  RVInfo.AnalyzeCallResult(Ins, RetCC_Sim);

  // Copy all of the result registers out of their specified physreg.
  for (auto &VA : RVLocs) {
    // Copy the value out
    SDValue RetValue =
        DAG.getCopyFromReg(Chain, dl, VA.getLocReg(), VA.getLocVT(), Glue);
    // Glue the RetValue to the end of the call sequence
    Chain = RetValue.getValue(1);
    Glue = RetValue.getValue(2);

    if (VA.getLocVT() == MVT::i32 && VA.getValVT() == MVT::f64) {
      llvm_unreachable("");
      // assert(VA.getLocReg() == ArgGPRs[0] && "Unexpected reg assignment");
      // SDValue RetValue2 =
      //     DAG.getCopyFromReg(Chain, DL, ArgGPRs[1], MVT::i32, Glue);
      // Chain = RetValue2.getValue(1);
      // Glue = RetValue2.getValue(2);
      // RetValue = DAG.getNode(RISCVISD::BuildPairF64, DL, MVT::f64, RetValue,
      //                        RetValue2);
    }

    RetValue = convertValVTToLocVT(DAG, RetValue, VA, dl);
    InVals.push_back(RetValue);
  }

  return Chain;
}

// FIXME? Maybe this could be a TableGen attribute on some registers and
// this table could be generated automatically from RegInfo.
Register SimTargetLowering::getRegisterByName(const char* RegName, LLT VT,
                                                const MachineFunction &MF) const {
  Register Reg = StringSwitch<Register>(RegName)
    .Case("r0", SIM::R0).Case("r1", SIM::R1).Case("r2", SIM::R2).Case("r3", SIM::R3)
    .Case("r4", SIM::R4).Case("r5", SIM::R5).Case("r6", SIM::R6).Case("r7", SIM::R7)
    .Case("r8", SIM::R8).Case("r9", SIM::R9).Case("r10", SIM::R10).Case("r11", SIM::R11)
    .Case("r12", SIM::R12).Case("r13", SIM::R13).Case("r14", SIM::R14).Case("r15", SIM::R15)
    .Default(0);

  if (Reg) {
    return Reg;
  }

  report_fatal_error("Invalid register name global variable");
}

//===----------------------------------------------------------------------===//
// TargetLowering Implementation
//===----------------------------------------------------------------------===//

SimTargetLowering::SimTargetLowering(const TargetMachine &TM,
                                     const SimSubtarget &STI)
    : TargetLowering(TM), Subtarget(&STI) {

  // Set up the register classes.
  addRegisterClass(MVT::i32, &SIM::GPRRegClass);
  addRegisterClass(MVT::f32, &SIM::GPRRegClass);

  computeRegisterProperties(Subtarget->getRegisterInfo());

  setStackPointerRegisterToSaveRestore(SIM::SP);

  for (unsigned Opc = 0; Opc < ISD::BUILTIN_OP_END; ++Opc) {
    setOperationAction(Opc, MVT::i32, Expand);
  }

  // TODO: add branches?
  setOperationAction(ISD::ADD, MVT::i32, Legal);
  setOperationAction(ISD::SUB, MVT::i32, Legal);
  setOperationAction(ISD::MUL, MVT::i32, Legal);
  setOperationAction(ISD::SDIV, MVT::i32, Legal);
  setOperationAction(ISD::SREM, MVT::i32, Legal);
  setOperationAction(ISD::UREM, MVT::i32, Expand);

  setOperationAction(ISD::AND, MVT::i32, Legal);
  setOperationAction(ISD::OR, MVT::i32, Legal);
  setOperationAction(ISD::XOR, MVT::i32, Legal);

  setOperationAction(ISD::SHL, MVT::i32, Legal);
  setOperationAction(ISD::SRA, MVT::i32, Legal);
  setOperationAction(ISD::SRL, MVT::i32, Legal);

  setOperationAction(ISD::LOAD, MVT::i32, Legal);
  setOperationAction(ISD::STORE, MVT::i32, Legal);

  // don't set Custom ConstantPool, instead match constants with pattern in InstrInfo.td
  setOperationAction(ISD::Constant, MVT::i32, Legal);
  setOperationAction(ISD::UNDEF, MVT::i32, Legal);

  // TODO: try not to expand BRCOND
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC, MVT::i32, Custom);

  setOperationAction(ISD::FRAMEADDR, MVT::i32, Legal);

  setOperationAction(ISD::SELECT, MVT::i32, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i32, Custom);
  // setTargetDAGCombine(ISD::BITCAST);

  // setOperationAction(ISD::INTRINSIC_WO_CHAIN, MVT::Other, Custom);

  // setMinFunctionAlignment(Align(4));
}

const char *SimTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((SIMISD::NodeType)Opcode) {
  case SIMISD::FIRST_NUMBER:    break;
  case SIMISD::RET:             return "SIMISD::RET";
  case SIMISD::CALL:            return "SIMISD::CALL";
  case SIMISD::BR_CC:           return "SIMISD::BR_CC";
  case SIMISD::SELECT_CC:       return "SIMISD::SELECT_CC";
  }
  return nullptr;
}

EVT SimTargetLowering::getSetCCResultType(const DataLayout &, LLVMContext &,
                                          EVT VT) const {
  assert(!VT.isVector() && "can't support vector type");
  return MVT::i32;
}

// taken from RISCV
bool SimTargetLowering::isLegalAddressingMode(const DataLayout &DL,
                                              const AddrMode &AM, Type *Ty,
                                              unsigned AS,
                                              Instruction *I) const {
  // No global is ever allowed as a base.
  if (AM.BaseGV)
    return false;

  if (!isInt<16>(AM.BaseOffs))
    return false;

  switch (AM.Scale) {
  case 0: // "r+i" or just "i", depending on HasBaseReg.
    break;
  case 1:
    if (!AM.HasBaseReg) // allow "r+i".
      break;
    return false; // disallow "r+r" or "r+r+i".
  default:
    return false;
  }

  return true;
}

MachineBasicBlock *
SimTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                               MachineBasicBlock *BB) const {
  switch (MI.getOpcode()) {
  default: llvm_unreachable("Unknown SELECT_CC!");
  case SIM::PseudoSELECT_CC:
    return expandSelectCC(MI, BB);
  }
}

static unsigned convertCondCodeToInstruction(unsigned CC) {
  unsigned BROpcode = 0;
  switch(CC) {
  default:
    errs() << "unsupported CondCode in branch: " << CC << '\n';
    llvm_unreachable("");
    break;
  case ISD::CondCode::SETUEQ:
  case ISD::CondCode::SETEQ:
    BROpcode = SIM::BEQ;
    break;
  case ISD::CondCode::SETUNE:
  case ISD::CondCode::SETNE:
    BROpcode = SIM::BNE;
    break;
  case ISD::CondCode::SETULE:
  case ISD::CondCode::SETLE:
    BROpcode = SIM::BLE;
    break;
  case ISD::CondCode::SETUGT:
  case ISD::CondCode::SETGT:
    BROpcode = SIM::BGT;
    break;
  }
  return BROpcode;
}

MachineBasicBlock *
SimTargetLowering::expandSelectCC(MachineInstr &MI, MachineBasicBlock *BB) const {
  const TargetInstrInfo &TII = *Subtarget->getInstrInfo();
  DebugLoc dl = MI.getDebugLoc();
  unsigned CC = (ISD::CondCode)MI.getOperand(5).getImm();
  auto LHS = MI.getOperand(1).getReg();
  auto RHS = MI.getOperand(2).getReg();
  switch (CC) {
  default:
    break;
  case ISD::SETLT:
  case ISD::SETGE:
  case ISD::SETULT:
  case ISD::SETUGE:
    CC = ISD::getSetCCSwappedOperands((ISD::CondCode)MI.getOperand(5).getImm());
    std::swap(LHS, RHS);
    break;
  }

  unsigned BROpcode = convertCondCodeToInstruction(CC);

  // To "insert" a SELECT_CC instruction, we actually have to insert the
  // triangle control-flow pattern. The incoming instruction knows the
  // destination vreg to set, the condition code register to branch on, the
  // true/false values to select between, and the condition code for the branch.
  //
  // We produce the following control flow:
  //     ThisMBB
  //     |  \
  //     |  IfFalseMBB
  //     | /
  //    SinkMBB
  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineFunction::iterator It = ++BB->getIterator();

  MachineBasicBlock *ThisMBB = BB;
  MachineFunction *F = BB->getParent();
  MachineBasicBlock *IfFalseMBB = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *SinkMBB = F->CreateMachineBasicBlock(LLVM_BB);
  F->insert(It, IfFalseMBB);
  F->insert(It, SinkMBB);

  // Transfer the remainder of ThisMBB and its successor edges to SinkMBB.
  SinkMBB->splice(SinkMBB->begin(), ThisMBB,
                  std::next(MachineBasicBlock::iterator(MI)), ThisMBB->end());
  SinkMBB->transferSuccessorsAndUpdatePHIs(ThisMBB);

  // Set the new successors for ThisMBB.
  ThisMBB->addSuccessor(IfFalseMBB);
  ThisMBB->addSuccessor(SinkMBB);

  BuildMI(ThisMBB, dl, TII.get(BROpcode))
    .addReg(LHS)
    .addReg(MI.getOperand(2).getReg())
    .addMBB(SinkMBB);

  // IfFalseMBB just falls through to SinkMBB.
  IfFalseMBB->addSuccessor(SinkMBB);

  // %Result = phi [ %TrueValue, ThisMBB ], [ %FalseValue, IfFalseMBB ]
  BuildMI(*SinkMBB, SinkMBB->begin(), dl, TII.get(SIM::PHI),
          MI.getOperand(0).getReg())
      .addReg(MI.getOperand(3).getReg())
      .addMBB(ThisMBB)
      .addReg(MI.getOperand(4).getReg())
      .addMBB(IfFalseMBB);

  MI.eraseFromParent(); // The pseudo instruction is gone now.
  return SinkMBB;
}

static SDValue LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG,
                              const SimSubtarget *Subtarget) {
  const auto &RI = *Subtarget->getRegisterInfo();
  auto &MF = DAG.getMachineFunction();
  auto &MFI = MF.getFrameInfo();
  MFI.setFrameAddressIsTaken(true);
  Register FrameReg = RI.getFrameRegister(MF);
  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  SDValue FrameAddr = DAG.getCopyFromReg(DAG.getEntryNode(), DL, FrameReg, VT);
  assert((cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue() == 0) && "only for current frame");
  return FrameAddr;
}

static void translateSetCCForBranch(const SDLoc &DL, SDValue &LHS, SDValue &RHS,
                                    ISD::CondCode &CC, SelectionDAG &DAG) {
  switch (CC) {
  default:
    break;
  case ISD::SETLT:
  case ISD::SETGE:
  case ISD::SETULT:
  case ISD::SETUGE:
    // We don't implement BLT and BGE instructions - must replace them on opposite
    CC = ISD::getSetCCSwappedOperands(CC);
    std::swap(LHS, RHS);
    break;
  }
}

static void translateUnorderedCondCodeForBranch(ISD::CondCode &CC) {
  switch (CC) {
  default:
    break;
  case ISD::SETUEQ:
    CC = ISD::SETEQ;
    break;
  case ISD::SETUNE:
    CC = ISD::SETNE;
    break;
  case ISD::SETULE:
    CC = ISD::SETLE;
    break;
  case ISD::SETUGT:
    CC = ISD::SETGT;
    break;
  case ISD::SETUGE:
    CC = ISD::SETGE;
    break;
  case ISD::SETULT:
    CC = ISD::SETLT;
    break;
  }
}

static SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) {
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  translateUnorderedCondCodeForBranch(CC);
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);
  SDLoc dl(Op);

  assert(LHS.getValueType() == MVT::i32);

  translateSetCCForBranch(dl, LHS, RHS, CC, DAG);

  SDValue TargetCC = DAG.getCondCode(CC);
  return DAG.getNode(SIMISD::BR_CC, dl, Op.getValueType(), Op.getOperand(0),
                     LHS, RHS, TargetCC, Dest);
}

static SDValue LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();
  SDValue TrueVal = Op.getOperand(2);
  SDValue FalseVal = Op.getOperand(3);
  SDLoc dl(Op);

  if (LHS.getValueType() != MVT::i32
      || RHS.getValueType() != MVT::i32
      || TrueVal.getValueType() != MVT::i32
      || FalseVal.getValueType() != MVT::i32) {
    llvm_unreachable("must be integer type");
  }

  SDValue TargetCC = DAG.getConstant(CC, dl, MVT::i32);
  return DAG.getNode(SIMISD::SELECT_CC, dl, TrueVal.getValueType(),
                     {LHS, RHS, TrueVal, FalseVal, TargetCC});
}

SDValue SimTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Should not custom lower this!");
  case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG,
                                                      Subtarget);
  case ISD::BR_CC:              return LowerBR_CC(Op, DAG);
  case ISD::SELECT_CC:          return LowerSELECT_CC(Op, DAG);
  }
}

SDValue SimTargetLowering::PerformDAGCombine(SDNode *N,
                                             DAGCombinerInfo &DCI) const {
  // TODO: do smth smart
  // switch (N->getOpcode()) {
  // default:
  //   break;
  // case ISD::BITCAST:
  //   return PerformBITCASTCombine(N, DCI);
  // }
  return SDValue();
}
