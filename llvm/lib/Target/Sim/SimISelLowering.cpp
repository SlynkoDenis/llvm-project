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

// The calling conventions in SimCallingConv.td are described in terms of the
// callee's register window. This function translates registers to the
// corresponding caller window %o register.
static unsigned toCallerWindow(unsigned Reg) {
  // TODO: implement
  // static_assert(SIM::I0 + 7 == SIM::I7 && SIM::O0 + 7 == SIM::O7,
  //               "Unexpected enum");
  // if (Reg >= SIM::I0 && Reg <= SIM::I7)
  //   return Reg - SIM::I0 + SIM::O0;
  return Reg;
}

static SDValue convertValVTToLocVT(SelectionDAG &DAG, SDValue Val,
                                   const CCValAssign &VA, const SDLoc &DL) {
  EVT LocVT = VA.getLocVT();

  if (VA.getValVT() == MVT::f32) {
    llvm_unreachable("TBD");
  }

  // Promote the value if needed.
  switch (VA.getLocInfo()) {
  default:
    llvm_unreachable("Unexpected LocInfo");
  case CCValAssign::Full:
    break;
  case CCValAssign::SExt:
    llvm_unreachable("TBD");
    Val = DAG.getNode(ISD::SIGN_EXTEND, DL, LocVT, Val);
    break;
  case CCValAssign::ZExt:
    llvm_unreachable("TBD");
    Val = DAG.getNode(ISD::ZERO_EXTEND, DL, LocVT, Val);
    break;
  case CCValAssign::BCvt:
    llvm_unreachable("TBD");
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

    // TODO: delete it
    // if (VA.needsCustom()) {
    //   assert(VA.getLocVT() == MVT::v2i32);
    //   // Legalize ret v2i32 -> ret 2 x i32 (Basically: do what would
    //   // happen by default if this wasn't a legal type)

    //   SDValue Part0 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, DL, MVT::i32,
    //                               Arg,
    //                               DAG.getConstant(0, DL, getVectorIdxTy(DAG.getDataLayout())));
    //   SDValue Part1 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, DL, MVT::i32,
    //                               Arg,
    //                               DAG.getConstant(1, DL, getVectorIdxTy(DAG.getDataLayout())));

    //   Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Part0, Flag);
    //   Flag = Chain.getValue(1);
    //   RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
    //   VA = RVLocs[++i]; // skip ahead to next loc
    //   Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Part1,
    //                            Flag);
    // } else {
    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Arg, Flag);
    // }

    // Guarantee that all emitted copies are stuck together with flags.
    // TODO: this action is redundant for Simulation model - we don't need Glue
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  // If the function returns a struct, copy the SRetReturnReg to I0
  if (MF.getFunction().hasStructRetAttr()) {
    llvm_unreachable("TBD");
    // SimMachineFunctionInfo *SFI = MF.getInfo<SimMachineFunctionInfo>();
    // Register Reg = SFI->getSRetReturnReg();
    // if (!Reg) {
    //   llvm_unreachable("sret virtual register not created in the entry block");
    // }
    // auto PtrVT = getPointerTy(DAG.getDataLayout());
    // SDValue Val = DAG.getCopyFromReg(Chain, DL, Reg, PtrVT);
    // Chain = DAG.getCopyToReg(Chain, DL, SIM::SP, Val, Flag);
    // Flag = Chain.getValue(1);
    // RetOps.push_back(DAG.getRegister(SIM::R14, PtrVT));
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
  if (!CCInfo.CheckReturn(Outs, RetCC_Sim))
    return false;
  if (CCInfo.getNextStackOffset() != 0 && IsVarArg)
    llvm_unreachable("");
  return true;
}


SDValue SimTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  SimMachineFunctionInfo *FuncInfo = MF.getInfo<SimMachineFunctionInfo>();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_Sim);

  const unsigned StackOffset = 92;

  unsigned InIdx = 0;
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i, ++InIdx) {
    CCValAssign &VA = ArgLocs[i];

    // TODO: do u need it?
    // if (Ins[InIdx].Flags.isSRet()) {
    //   if (InIdx != 0)
    //     report_fatal_error("Sim only supports sret on the first parameter");
    //   // Get SRet from [%fp+64].
    //   int FrameIdx = MF.getFrameInfo().CreateFixedObject(4, 64, true);
    //   SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);
    //   SDValue Arg =
    //       DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
    //   InVals.push_back(Arg);
    //   continue;
    // }

    if (VA.isRegLoc()) {
      Register VReg = RegInfo.createVirtualRegister(&SIM::GPRRegClass);
      MF.getRegInfo().addLiveIn(VA.getLocReg(), VReg);
      SDValue Arg = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i32);
      if (VA.getLocVT() == MVT::f32)
        Arg = DAG.getNode(ISD::BITCAST, dl, MVT::f32, Arg);
      else if (VA.getLocVT() != MVT::i32) {
        Arg = DAG.getNode(ISD::AssertSext, dl, MVT::i32, Arg,
                          DAG.getValueType(VA.getLocVT()));
        Arg = DAG.getNode(ISD::TRUNCATE, dl, VA.getLocVT(), Arg);
      }
      InVals.push_back(Arg);
      continue;
    }

    assert(VA.isMemLoc());

    unsigned Offset = VA.getLocMemOffset() + StackOffset;
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
  }

  if (MF.getFunction().hasStructRetAttr()) {
    llvm_unreachable("TBD: LowerFormalArguments for hasStructRetAttr Function");
    // Copy the SRet Argument to SRetReturnReg.
    SimMachineFunctionInfo *SFI = MF.getInfo<SimMachineFunctionInfo>();
    Register Reg = SFI->getSRetReturnReg();
    if (!Reg) {
      Reg = MF.getRegInfo().createVirtualRegister(&SIM::GPRRegClass);
      SFI->setSRetReturnReg(Reg);
    }
    SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), dl, Reg, InVals[0]);
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, Copy, Chain);
  }

  // Store remaining ArgRegs to the stack if this is a varargs function.
  if (isVarArg) {
    llvm_unreachable("TBD: LowerFormalArguments for isVarArg");
    // TODO: change regs?
    static const MCPhysReg ArgRegs[] = {
      SIM::R10, SIM::R11, SIM::R12, SIM::R13, SIM::R14, SIM::R15
    };
    unsigned NumAllocated = CCInfo.getFirstUnallocated(ArgRegs);
    const MCPhysReg *CurArgReg = ArgRegs + NumAllocated;
    const MCPhysReg *ArgRegEnd = ArgRegs + 6;
    unsigned ArgOffset = CCInfo.getNextStackOffset();
    if (NumAllocated == 6) {
      ArgOffset += StackOffset;
    } else {
      assert(!ArgOffset);
      ArgOffset = 68 + 4 * NumAllocated;
    }

    // Remember the vararg offset for the va_start implementation.
    FuncInfo->setVarArgsFrameOffset(ArgOffset);

    std::vector<SDValue> OutChains;

    for (; CurArgReg != ArgRegEnd; ++CurArgReg) {
      Register VReg = RegInfo.createVirtualRegister(&SIM::GPRRegClass);
      MF.getRegInfo().addLiveIn(*CurArgReg, VReg);
      SDValue Arg = DAG.getCopyFromReg(DAG.getRoot(), dl, VReg, MVT::i32);

      int FrameIdx = MF.getFrameInfo().CreateFixedObject(4, ArgOffset,
                                                         true);
      SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);

      OutChains.push_back(
          DAG.getStore(DAG.getRoot(), dl, Arg, FIPtr, MachinePointerInfo()));
      ArgOffset += 4;
    }

    if (!OutChains.empty()) {
      OutChains.push_back(Chain);
      Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains);
    }
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
  isTailCall = false;

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_Sim);

  // Get the size of the outgoing arguments stack space requirement.
  unsigned ArgsSize = CCInfo.getNextStackOffset();

  // TODO: do u need it?
  // Keep stack frames 8-byte aligned.
  // ArgsSize = (ArgsSize + 7) & ~7;

  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();

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
                            false,        // isVolatile,
                            (Size <= 32), // AlwaysInline if size <= 32,
                            isTailCall,        // isTailCall
                            MachinePointerInfo(), MachinePointerInfo());
      ByValArgs.push_back(FIPtr);
    }
    else {
      SDValue nullVal;
      ByValArgs.push_back(nullVal);
    }
  }

  Chain = DAG.getCALLSEQ_START(Chain, ArgsSize, 0, dl);

  SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;

  // TODO: remove this offset
  const unsigned StackOffset = 92;
  bool hasStructRetAttr = false;
  unsigned SRetArgSize = 0;
  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned i = 0, realArgIdx = 0, byvalArgIdx = 0, e = ArgLocs.size();
       i != e;
       ++i, ++realArgIdx) {
    CCValAssign &VA = ArgLocs[i];
    SDValue Arg = OutVals[realArgIdx];

    ISD::ArgFlagsTy Flags = Outs[realArgIdx].Flags;

    // Use local copy if it is a byval arg.
    if (Flags.isByVal()) {
      Arg = ByValArgs[byvalArgIdx++];
      if (!Arg) {
        continue;
      }
    }

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
    default: llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full: break;
    case CCValAssign::SExt:
      Arg = DAG.getNode(ISD::SIGN_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    case CCValAssign::BCvt:
      Arg = DAG.getNode(ISD::BITCAST, dl, VA.getLocVT(), Arg);
      break;
    }

    // TODO: do u need it?
    // if (Flags.isSRet()) {
    //   assert(VA.needsCustom());
    //   // store SRet argument in %sp+64
    //   SDValue StackPtr = DAG.getRegister(SIM::SP, MVT::i32);
    //   SDValue PtrOff = DAG.getIntPtrConstant(64, dl);
    //   PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
    //   MemOpChains.push_back(
    //       DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
    //   hasStructRetAttr = true;
    //   // sret only allowed on first argument
    //   assert(Outs[realArgIdx].OrigArgIndex == 0);
    //   PointerType *Ty = cast<PointerType>(CLI.getArgs()[0].Ty);
    //   Type *ElementTy = Ty->getElementType();
    //   SRetArgSize = DAG.getDataLayout().getTypeAllocSize(ElementTy);
    //   continue;
    // }

    if (VA.needsCustom()) {
      llvm_unreachable("TBD - LowerCall");
      // assert(VA.getLocVT() == MVT::f64 || VA.getLocVT() == MVT::v2i32);

      // if (VA.isMemLoc()) {
      //   unsigned Offset = VA.getLocMemOffset() + StackOffset;
      //   // if it is double-word aligned, just store.
      //   if (Offset % 8 == 0) {
      //     SDValue StackPtr = DAG.getRegister(SIM::SP, MVT::i32);
      //     SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
      //     PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
      //     MemOpChains.push_back(
      //         DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
      //     continue;
      //   }
      // }

      // if (VA.getLocVT() == MVT::f64) {
      //   // Move from the float value from float registers into the
      //   // integer registers.
      //   if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(Arg))
      //     Arg = bitcastConstantFPToInt(C, dl, DAG);
      //   else
      //     Arg = DAG.getNode(ISD::BITCAST, dl, MVT::v2i32, Arg);
      // }

      // SDValue Part0 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, MVT::i32,
      //                             Arg,
      //                             DAG.getConstant(0, dl, getVectorIdxTy(DAG.getDataLayout())));
      // SDValue Part1 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, MVT::i32,
      //                             Arg,
      //                             DAG.getConstant(1, dl, getVectorIdxTy(DAG.getDataLayout())));

      // if (VA.isRegLoc()) {
      //   RegsToPass.push_back(std::make_pair(VA.getLocReg(), Part0));
      //   assert(i+1 != e);
      //   CCValAssign &NextVA = ArgLocs[++i];
      //   if (NextVA.isRegLoc()) {
      //     RegsToPass.push_back(std::make_pair(NextVA.getLocReg(), Part1));
      //   } else {
      //     // Store the second part in stack.
      //     unsigned Offset = NextVA.getLocMemOffset() + StackOffset;
      //     SDValue StackPtr = DAG.getRegister(SIM::SP, MVT::i32);
      //     SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
      //     PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
      //     MemOpChains.push_back(
      //         DAG.getStore(Chain, dl, Part1, PtrOff, MachinePointerInfo()));
      //   }
      // } else {
      //   unsigned Offset = VA.getLocMemOffset() + StackOffset;
      //   // Store the first part.
      //   SDValue StackPtr = DAG.getRegister(SIM::SP, MVT::i32);
      //   SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
      //   PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
      //   MemOpChains.push_back(
      //       DAG.getStore(Chain, dl, Part0, PtrOff, MachinePointerInfo()));
      //   // Store the second part.
      //   PtrOff = DAG.getIntPtrConstant(Offset + 4, dl);
      //   PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
      //   MemOpChains.push_back(
      //       DAG.getStore(Chain, dl, Part1, PtrOff, MachinePointerInfo()));
      // }
      // continue;
    }

    // Arguments that can be passed on register must be kept at
    // RegsToPass vector
    if (VA.isRegLoc()) {
      if (VA.getLocVT() != MVT::f32) {
        RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
        continue;
      }
      Arg = DAG.getNode(ISD::BITCAST, dl, MVT::i32, Arg);
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
      continue;
    }

    assert(VA.isMemLoc());

    // Create a store off the stack pointer for this argument.
    SDValue StackPtr = DAG.getRegister(SIM::SP, MVT::i32);
    SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset() + StackOffset,
                                           dl);
    PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
    MemOpChains.push_back(
        DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
  }

  // Emit all stores, make sure the occur before any copies into physregs.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOpChains);

  // Build a sequence of copy-to-reg nodes chained together with token
  // chain and flag operands which copy the outgoing args into registers.
  // The InFlag in necessary since all emitted instructions must be
  // stuck together.
  SDValue InFlag;
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Register Reg = toCallerWindow(RegsToPass[i].first);
    Chain = DAG.getCopyToReg(Chain, dl, Reg, RegsToPass[i].second, InFlag);
    InFlag = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), dl, MVT::i32, 0);
  else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i32);

  // Returns a chain & a flag for retval copy to use
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);
  if (hasStructRetAttr)
    Ops.push_back(DAG.getTargetConstant(SRetArgSize, dl, MVT::i32));
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i)
    Ops.push_back(DAG.getRegister(toCallerWindow(RegsToPass[i].first),
                                  RegsToPass[i].second.getValueType()));

  // Add a register mask operand representing the call-preserved registers.
  const SimRegisterInfo *TRI = Subtarget->getRegisterInfo();
  const uint32_t *Mask =
      TRI->getRTCallPreservedMask(CallConv);
  assert(Mask && "Missing call preserved mask for calling convention");
  Ops.push_back(DAG.getRegisterMask(Mask));

  if (InFlag.getNode()) {
    Ops.push_back(InFlag);
  }

  Chain = DAG.getNode(SIMISD::CALL, dl, NodeTys, Ops);
  InFlag = Chain.getValue(1);

  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(ArgsSize, dl, true),
                             DAG.getIntPtrConstant(0, dl, true), InFlag, dl);
  InFlag = Chain.getValue(1);

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RVInfo(CallConv, isVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  RVInfo.AnalyzeCallResult(Ins, RetCC_Sim);

  // Copy all of the result registers out of their specified physreg.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    Chain =
        DAG.getCopyFromReg(Chain, dl, toCallerWindow(RVLocs[i].getLocReg()),
                           RVLocs[i].getValVT(), InFlag)
            .getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
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

  for (unsigned Opc = 0; Opc < ISD::BUILTIN_OP_END; ++Opc)
    setOperationAction(Opc, MVT::i32, Expand);

  setOperationAction(ISD::ADD, MVT::i32, Legal);
  setOperationAction(ISD::MUL, MVT::i32, Legal);

  setOperationAction(ISD::LOAD, MVT::i32, Legal);
  setOperationAction(ISD::STORE, MVT::i32, Legal);

  setOperationAction(ISD::Constant, MVT::i32, Legal);
  setOperationAction(ISD::UNDEF, MVT::i32, Legal);

  setOperationAction(ISD::BR_CC, MVT::i32, Custom);

  setOperationAction(ISD::FRAMEADDR, MVT::i32, Legal);
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
  }
  return nullptr;
}

EVT SimTargetLowering::getSetCCResultType(const DataLayout &, LLVMContext &,
                                            EVT VT) const {
  // TODO: do smth smart
  return MVT::i32;
  // if (!VT.isVector())
    // return MVT::i32;
  // return VT.changeVectorElementTypeToInteger();
}

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
  // Only for current frame
  assert(cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue() == 0);
  return FrameAddr;

}

static SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) {
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);
  SDLoc dl(Op);

  assert(LHS.getValueType() == MVT::i32);
  // TODO: resolve this possible call
  // If this is a br_cc of a "setcc", and if the setcc got lowered into
  // an CMP[IF]CC/SELECT_[IF]CC pair, find the original compared values.
  // LookThroughSetCC(LHS, RHS, CC, SPCC);

  if (CC == ISD::CondCode::SETGE) {
    CC = ISD::getSetCCSwappedOperands(CC);
    std::swap(LHS, RHS);
  }

  SDValue TargetCC = DAG.getCondCode(CC);
  return DAG.getNode(SIMISD::BR_CC, dl, Op.getValueType(), Op.getOperand(0),
                     LHS, RHS, TargetCC, Dest);
}

SDValue SimTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Should not custom lower this!");

  case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG,
                                                      Subtarget);
  case ISD::BR_CC:              return LowerBR_CC(Op, DAG);
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
