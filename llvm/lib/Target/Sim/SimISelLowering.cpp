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



//===----------------------------------------------------------------------===//
// Calling Convention Implementation
//===----------------------------------------------------------------------===//

// static bool CC_Sim_Assign_SRet(unsigned &ValNo, MVT &ValVT,
//                                  MVT &LocVT, CCValAssign::LocInfo &LocInfo,
//                                  ISD::ArgFlagsTy &ArgFlags, CCState &State)
// {
//   assert (ArgFlags.isSRet());

//   // Assign SRet argument.
//   State.addLoc(CCValAssign::getCustomMem(ValNo, ValVT,
//                                          0,
//                                          LocVT, LocInfo));
//   return true;
// }

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

SDValue
SimTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                               bool IsVarArg,
                               const SmallVectorImpl<ISD::OutputArg> &Outs,
                               const SmallVectorImpl<SDValue> &OutVals,
                               const SDLoc &DL, SelectionDAG &DAG) const {
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
  // Make room for the return address offset.
  RetOps.push_back(SDValue());

  // Copy the result values into the output registers.
  for (unsigned i = 0, realRVLocIdx = 0;
       i != RVLocs.size();
       ++i, ++realRVLocIdx) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    SDValue Arg = OutVals[realRVLocIdx];

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
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  unsigned RetAddrOffset = 8; // Call Inst + Delay Slot
  // If the function returns a struct, copy the SRetReturnReg to I0
  if (MF.getFunction().hasStructRetAttr()) {
    llvm_unreachable("TBD - LowerReturn");
    SimMachineFunctionInfo *SFI = MF.getInfo<SimMachineFunctionInfo>();
    Register Reg = SFI->getSRetReturnReg();
    if (!Reg) {
      llvm_unreachable("sret virtual register not created in the entry block");
    }
    auto PtrVT = getPointerTy(DAG.getDataLayout());
    SDValue Val = DAG.getCopyFromReg(Chain, DL, Reg, PtrVT);
    Chain = DAG.getCopyToReg(Chain, DL, SIM::SP, Val, Flag);
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(SIM::R14, PtrVT));
    RetAddrOffset = 12; // CallInst + Delay Slot + Unimp
  }

  RetOps[0] = Chain;  // Update chain.
  // TODO: do u need RetAddr?
  RetOps[1] = DAG.getConstant(RetAddrOffset, DL, MVT::i32);

  // Add the flag if we have it.
  if (Flag.getNode()) {
    RetOps.push_back(Flag);
  }

  return DAG.getNode(SIMISD::RET, DL, MVT::Other, RetOps);
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
    } else if (VA.getValVT() == MVT::f128) {
      report_fatal_error("Sim does not handle f128 in calls; "
                         "pass indirectly");
    } else {
      // We shouldn't see any other value types here.
      llvm_unreachable("Unexpected ValVT encountered in frame lowering.");
    }
    InVals.push_back(Load);
  }

  if (MF.getFunction().hasStructRetAttr()) {
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
    // TODO: change regs
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
  // Instructions which use registers as conditionals examine all the
  // bits (as does the pseudo SELECT_CC expansion). I don't think it
  // matters much whether it's ZeroOrOneBooleanContent, or
  // ZeroOrNegativeOneBooleanContent, so, arbitrarily choose the
  // former.
  // setBooleanContents(ZeroOrOneBooleanContent);
  // setBooleanVectorContents(ZeroOrOneBooleanContent);

  for (unsigned Opc = 0; Opc < ISD::BUILTIN_OP_END; ++Opc)
    setOperationAction(Opc, MVT::i32, Expand);

  // Set up the register classes.
  addRegisterClass(MVT::i32, &SIM::GPRRegClass);
  addRegisterClass(MVT::f32, &SIM::GPRRegClass);

  setOperationAction(ISD::ADD, MVT::i32, Legal);
  setOperationAction(ISD::MUL, MVT::i32, Legal);

  setOperationAction(ISD::LOAD, MVT::i32, Legal);
  setOperationAction(ISD::STORE, MVT::i32, Legal);

  setOperationAction(ISD::BR_CC, MVT::i32, Custom);

  setStackPointerRegisterToSaveRestore(SIM::SP);

  // setTargetDAGCombine(ISD::BITCAST);

  // setOperationAction(ISD::INTRINSIC_WO_CHAIN, MVT::Other, Custom);

  // setMinFunctionAlignment(Align(4));

  // computeRegisterProperties(Subtarget->getRegisterInfo());
}

const char *SimTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((SIMISD::NodeType)Opcode) {
  case SIMISD::FIRST_NUMBER:    break;
  case SIMISD::CMPICC:          return "SIMISD::CMPICC";
  case SIMISD::CMPFCC:          return "SIMISD::CMPFCC";
  case SIMISD::BRICC:           return "SIMISD::BRICC";
  case SIMISD::BRXCC:           return "SIMISD::BRXCC";
  case SIMISD::BRFCC:           return "SIMISD::BRFCC";
  case SIMISD::SELECT_ICC:      return "SIMISD::SELECT_ICC";
  case SIMISD::SELECT_XCC:      return "SIMISD::SELECT_XCC";
  case SIMISD::SELECT_FCC:      return "SIMISD::SELECT_FCC";
  case SIMISD::Hi:              return "SIMISD::Hi";
  case SIMISD::Lo:              return "SIMISD::Lo";
  case SIMISD::FTOI:            return "SIMISD::FTOI";
  case SIMISD::ITOF:            return "SIMISD::ITOF";
  case SIMISD::FTOX:            return "SIMISD::FTOX";
  case SIMISD::XTOF:            return "SIMISD::XTOF";
  case SIMISD::CALL:            return "SIMISD::CALL";
  case SIMISD::RET:             return "SIMISD::RET";
  case SIMISD::GLOBAL_BASE_REG: return "SIMISD::GLOBAL_BASE_REG";
  case SIMISD::FLUSHW:          return "SIMISD::FLUSHW";
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

// Convert to a target node and set target flags.
// SDValue SimTargetLowering::withTargetFlags(SDValue Op, unsigned TF,
//                                              SelectionDAG &DAG) const {
//   if (const GlobalAddressSDNode *GA = dyn_cast<GlobalAddressSDNode>(Op))
//     return DAG.getTargetGlobalAddress(GA->getGlobal(),
//                                       SDLoc(GA),
//                                       GA->getValueType(0),
//                                       GA->getOffset(), TF);

//   if (const ConstantPoolSDNode *CP = dyn_cast<ConstantPoolSDNode>(Op))
//     return DAG.getTargetConstantPool(CP->getConstVal(), CP->getValueType(0),
//                                      CP->getAlign(), CP->getOffset(), TF);

//   if (const BlockAddressSDNode *BA = dyn_cast<BlockAddressSDNode>(Op))
//     return DAG.getTargetBlockAddress(BA->getBlockAddress(),
//                                      Op.getValueType(),
//                                      0,
//                                      TF);

//   if (const ExternalSymbolSDNode *ES = dyn_cast<ExternalSymbolSDNode>(Op))
//     return DAG.getTargetExternalSymbol(ES->getSymbol(),
//                                        ES->getValueType(0), TF);

//   llvm_unreachable("Unhandled address SDNode");
// }

// Split Op into high and low parts according to HiTF and LoTF.
// Return an ADD node combining the parts.
// SDValue SimTargetLowering::makeHiLoPair(SDValue Op,
//                                         unsigned HiTF, unsigned LoTF,
//                                         SelectionDAG &DAG) const {
//   SDLoc DL(Op);
//   EVT VT = Op.getValueType();
//   SDValue Hi = DAG.getNode(SIMISD::Hi, DL, VT, withTargetFlags(Op, HiTF, DAG));
//   SDValue Lo = DAG.getNode(SIMISD::Lo, DL, VT, withTargetFlags(Op, LoTF, DAG));
//   return DAG.getNode(ISD::ADD, DL, VT, Hi, Lo);
// }

// Build SDNodes for producing an address from a GlobalAddress, ConstantPool,
// or ExternalSymbol SDNode.
// SDValue SimTargetLowering::makeAddress(SDValue Op, SelectionDAG &DAG) const {
//   SDLoc DL(Op);
//   EVT VT = getPointerTy(DAG.getDataLayout());

//   // Handle PIC mode first. Sim needs a got load for every variable!
//   if (isPositionIndependent()) {
//     const Module *M = DAG.getMachineFunction().getFunction().getParent();
//     PICLevel::Level picLevel = M->getPICLevel();
//     SDValue Idx;

//     if (picLevel == PICLevel::SmallPIC) {
//       // This is the pic13 code model, the GOT is known to be smaller than 8KiB.
//       Idx = DAG.getNode(SIMISD::Lo, DL, Op.getValueType(),
//                         withTargetFlags(Op, 0, DAG));
//     } else {
//       // This is the pic32 code model, the GOT is known to be smaller than 4GB.
//       Idx = makeHiLoPair(Op, SimMCExpr::VK_Sim_GOT22,
//                          SimMCExpr::VK_Sim_GOT10, DAG);
//     }

//     SDValue GlobalBase = DAG.getNode(SIMISD::GLOBAL_BASE_REG, DL, VT);
//     SDValue AbsAddr = DAG.getNode(ISD::ADD, DL, VT, GlobalBase, Idx);
//     // GLOBAL_BASE_REG codegen'ed with call. Inform MFI that this
//     // function has calls.
//     MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
//     MFI.setHasCalls(true);
//     return DAG.getLoad(VT, DL, DAG.getEntryNode(), AbsAddr,
//                        MachinePointerInfo::getGOT(DAG.getMachineFunction()));
//   }

//   // This is one of the absolute code models.
//   switch(getTargetMachine().getCodeModel()) {
//   default:
//     llvm_unreachable("Unsupported absolute code model");
//   case CodeModel::Small:
//     // abs32.
//     return makeHiLoPair(Op, SimMCExpr::VK_Sim_HI,
//                         SimMCExpr::VK_Sim_LO, DAG);
//   case CodeModel::Medium: {
//     // abs44.
//     SDValue H44 = makeHiLoPair(Op, SimMCExpr::VK_Sim_H44,
//                                SimMCExpr::VK_Sim_M44, DAG);
//     H44 = DAG.getNode(ISD::SHL, DL, VT, H44, DAG.getConstant(12, DL, MVT::i32));
//     SDValue L44 = withTargetFlags(Op, 0, DAG);
//     L44 = DAG.getNode(SIMISD::Lo, DL, VT, L44);
//     return DAG.getNode(ISD::ADD, DL, VT, H44, L44);
//   }
//   case CodeModel::Large: {
//     // abs64.
//     SDValue Hi = makeHiLoPair(Op, SimMCExpr::VK_Sim_HH,
//                               SimMCExpr::VK_Sim_HM, DAG);
//     Hi = DAG.getNode(ISD::SHL, DL, VT, Hi, DAG.getConstant(32, DL, MVT::i32));
//     SDValue Lo = makeHiLoPair(Op, SimMCExpr::VK_Sim_HI,
//                               SimMCExpr::VK_Sim_LO, DAG);
//     return DAG.getNode(ISD::ADD, DL, VT, Hi, Lo);
//   }
//   }
// }

// SDValue
// SimTargetLowering::LowerF128Op(SDValue Op, SelectionDAG &DAG,
//                                  const char *LibFuncName,
//                                  unsigned numArgs) const {

//   ArgListTy Args;

//   MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
//   auto PtrVT = getPointerTy(DAG.getDataLayout());

//   SDValue Callee = DAG.getExternalSymbol(LibFuncName, PtrVT);
//   Type *RetTy = Op.getValueType().getTypeForEVT(*DAG.getContext());
//   Type *RetTyABI = RetTy;
//   SDValue Chain = DAG.getEntryNode();
//   SDValue RetPtr;

//   if (RetTy->isFP128Ty()) {
//     // Create a Stack Object to receive the return value of type f128.
//     ArgListEntry Entry;
//     int RetFI = MFI.CreateStackObject(16, Align(8), false);
//     RetPtr = DAG.getFrameIndex(RetFI, PtrVT);
//     Entry.Node = RetPtr;
//     Entry.Ty   = PointerType::getUnqual(RetTy);
//     Entry.IsSRet = true;
//     Entry.IsReturned = false;
//     Args.push_back(Entry);
//     RetTyABI = Type::getVoidTy(*DAG.getContext());
//   }

//   assert(Op->getNumOperands() >= numArgs && "Not enough operands!");
//   for (unsigned i = 0, e = numArgs; i != e; ++i) {
//     // Chain = LowerF128_LibCallArg(Chain, Args, Op.getOperand(i), SDLoc(Op), DAG);
//   }
//   TargetLowering::CallLoweringInfo CLI(DAG);
//   CLI.setDebugLoc(SDLoc(Op)).setChain(Chain)
//     .setCallee(CallingConv::C, RetTyABI, Callee, std::move(Args));

//   std::pair<SDValue, SDValue> CallInfo = LowerCallTo(CLI);

//   // chain is in second result.
//   if (RetTyABI == RetTy)
//     return CallInfo.first;

//   assert (RetTy->isFP128Ty() && "Unexpected return type!");

//   Chain = CallInfo.second;

//   // Load RetPtr to get the return value.
//   return DAG.getLoad(Op.getValueType(), SDLoc(Op), Chain, RetPtr,
//                      MachinePointerInfo(), Align(8));
// }

// static SDValue LowerFP_TO_SINT(SDValue Op, SelectionDAG &DAG,
//                                const SimTargetLowering &TLI) {
//   SDLoc dl(Op);
//   EVT VT = Op.getValueType();
//   assert(VT == MVT::i32 || VT == MVT::i64);
//   assert(Op.getOperand(0).getValueType() != MVT::f128);

//   // Expand if the resulting type is illegal.
//   if (!TLI.isTypeLegal(VT))
//     return SDValue();

//   // Otherwise, Convert the fp value to integer in an FP register.
//   if (VT == MVT::i32)
//     Op = DAG.getNode(SIMISD::FTOI, dl, MVT::f32, Op.getOperand(0));
//   else
//     Op = DAG.getNode(SIMISD::FTOX, dl, MVT::f64, Op.getOperand(0));

//   return DAG.getNode(ISD::BITCAST, dl, VT, Op);
// }

// static SDValue LowerSINT_TO_FP(SDValue Op, SelectionDAG &DAG,
//                                const SimTargetLowering &TLI) {
//   SDLoc dl(Op);
//   EVT OpVT = Op.getOperand(0).getValueType();
//   assert(OpVT == MVT::i32 || (OpVT == MVT::i64));

//   EVT floatVT = (OpVT == MVT::i32) ? MVT::f32 : MVT::f64;

//   assert(Op.getValueType() != MVT::f128);

//   // Expand if the operand type is illegal.
//   if (!TLI.isTypeLegal(OpVT))
//     return SDValue();

//   // Otherwise, Convert the int value to FP in an FP register.
//   SDValue Tmp = DAG.getNode(ISD::BITCAST, dl, floatVT, Op.getOperand(0));
//   unsigned opcode = (OpVT == MVT::i32)? SIMISD::ITOF : SIMISD::XTOF;
//   return DAG.getNode(opcode, dl, Op.getValueType(), Tmp);
// }

// static SDValue LowerFP_TO_UINT(SDValue Op, SelectionDAG &DAG,
//                                const SimTargetLowering &TLI) {
//   SDLoc dl(Op);
//   EVT VT = Op.getValueType();

//   // Expand if it does not involve f128 or the target has support for
//   // quad floating point instructions and the resulting type is legal.
//   if (Op.getOperand(0).getValueType() != MVT::f128 ||
//       TLI.isTypeLegal(VT))
//     return SDValue();

//   assert(VT == MVT::i32 || VT == MVT::i64);
//   llvm_unreachable("TBD");
// }

// static SDValue LowerUINT_TO_FP(SDValue Op, SelectionDAG &DAG,
//                                const SimTargetLowering &TLI) {
//   SDLoc dl(Op);
//   EVT OpVT = Op.getOperand(0).getValueType();
//   assert(OpVT == MVT::i32 || OpVT == MVT::i64);

//   // Expand if it does not involve f128 or the target has support for
//   // quad floating point instructions and the operand type is legal.
//   if (Op.getValueType() != MVT::f128 || TLI.isTypeLegal(OpVT))
//     return SDValue();

//   llvm_unreachable("TBD");
// }

// static SDValue LowerVAARG(SDValue Op, SelectionDAG &DAG) {
//   SDNode *Node = Op.getNode();
//   EVT VT = Node->getValueType(0);
//   SDValue InChain = Node->getOperand(0);
//   SDValue VAListPtr = Node->getOperand(1);
//   EVT PtrVT = VAListPtr.getValueType();
//   const Value *SV = cast<SrcValueSDNode>(Node->getOperand(2))->getValue();
//   SDLoc DL(Node);
//   SDValue VAList =
//       DAG.getLoad(PtrVT, DL, InChain, VAListPtr, MachinePointerInfo(SV));
//   // Increment the pointer, VAList, to the next vaarg.
//   SDValue NextPtr = DAG.getNode(ISD::ADD, DL, PtrVT, VAList,
//                                 DAG.getIntPtrConstant(VT.getSizeInBits()/8,
//                                                       DL));
//   // Store the incremented VAList to the legalized pointer.
//   InChain = DAG.getStore(VAList.getValue(1), DL, NextPtr, VAListPtr,
//                          MachinePointerInfo(SV));
//   // Load the actual argument out of the pointer VAList.
//   // We can't count on greater alignment than the word size.
//   return DAG.getLoad(
//       VT, DL, InChain, VAList, MachinePointerInfo(),
//       std::min(PtrVT.getFixedSizeInBits(), VT.getFixedSizeInBits()) / 8);
// }

// static SDValue getFLUSHW(SDValue Op, SelectionDAG &DAG) {
//   SDLoc dl(Op);
//   SDValue Chain = DAG.getNode(SIMISD::FLUSHW,
//                               dl, MVT::Other, DAG.getEntryNode());
//   return Chain;
// }

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

// static SDValue LowerF64Op(SDValue SrcReg64, const SDLoc &dl, SelectionDAG &DAG,
//                           unsigned opcode) {
//   assert(SrcReg64.getValueType() == MVT::f64 && "LowerF64Op called on non-double!");
//   assert(opcode == ISD::FNEG || opcode == ISD::FABS);

//   // Lower fneg/fabs on f64 to fneg/fabs on f32.
//   // fneg f64 => fneg f32:sub_even, fmov f32:sub_odd.
//   // fabs f64 => fabs f32:sub_even, fmov f32:sub_odd.

//   // Note: in little-endian, the floating-point value is stored in the
//   // registers are in the opposite order, so the subreg with the sign
//   // bit is the highest-numbered (odd), rather than the
//   // lowest-numbered (even).

//   SDValue Hi32 = DAG.getTargetExtractSubreg(SIM::sub_even, dl, MVT::f32,
//                                             SrcReg64);
//   SDValue Lo32 = DAG.getTargetExtractSubreg(SIM::sub_odd, dl, MVT::f32,
//                                             SrcReg64);

//   if (DAG.getDataLayout().isLittleEndian())
//     Lo32 = DAG.getNode(opcode, dl, MVT::f32, Lo32);
//   else
//     Hi32 = DAG.getNode(opcode, dl, MVT::f32, Hi32);

//   SDValue DstReg64 = SDValue(DAG.getMachineNode(TargetOpcode::IMPLICIT_DEF,
//                                                 dl, MVT::f64), 0);
//   DstReg64 = DAG.getTargetInsertSubreg(SIM::sub_even, dl, MVT::f64,
//                                        DstReg64, Hi32);
//   DstReg64 = DAG.getTargetInsertSubreg(SIM::sub_odd, dl, MVT::f64,
//                                        DstReg64, Lo32);
//   return DstReg64;
// }

// Lower a f128 load into two f64 loads.
// static SDValue LowerF128Load(SDValue Op, SelectionDAG &DAG)
// {
//   // TODO: remove function
//   assert(false);
//   SDLoc dl(Op);
//   LoadSDNode *LdNode = cast<LoadSDNode>(Op.getNode());
//   assert(LdNode->getOffset().isUndef() && "Unexpected node type");

//   Align Alignment = commonAlignment(LdNode->getOriginalAlign(), 8);

//   SDValue Hi64 =
//       DAG.getLoad(MVT::f64, dl, LdNode->getChain(), LdNode->getBasePtr(),
//                   LdNode->getPointerInfo(), Alignment);
//   EVT addrVT = LdNode->getBasePtr().getValueType();
//   SDValue LoPtr = DAG.getNode(ISD::ADD, dl, addrVT,
//                               LdNode->getBasePtr(),
//                               DAG.getConstant(8, dl, addrVT));
//   SDValue Lo64 = DAG.getLoad(MVT::f64, dl, LdNode->getChain(), LoPtr,
//                              LdNode->getPointerInfo().getWithOffset(8),
//                              Alignment);

//   SDValue SubRegEven = DAG.getTargetConstant(SIM::sub_even64, dl, MVT::i32);
//   SDValue SubRegOdd  = DAG.getTargetConstant(SIM::sub_odd64, dl, MVT::i32);

//   SDNode *InFP128 = DAG.getMachineNode(TargetOpcode::IMPLICIT_DEF,
//                                        dl, MVT::f128);
//   InFP128 = DAG.getMachineNode(TargetOpcode::INSERT_SUBREG, dl,
//                                MVT::f128,
//                                SDValue(InFP128, 0),
//                                Hi64,
//                                SubRegEven);
//   InFP128 = DAG.getMachineNode(TargetOpcode::INSERT_SUBREG, dl,
//                                MVT::f128,
//                                SDValue(InFP128, 0),
//                                Lo64,
//                                SubRegOdd);
//   SDValue OutChains[2] = { SDValue(Hi64.getNode(), 1),
//                            SDValue(Lo64.getNode(), 1) };
//   SDValue OutChain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains);
//   SDValue Ops[2] = {SDValue(InFP128,0), OutChain};
//   return DAG.getMergeValues(Ops, dl);
// }

// static SDValue LowerLOAD(SDValue Op, SelectionDAG &DAG)
// {
//   LoadSDNode *LdNode = cast<LoadSDNode>(Op.getNode());

//   EVT MemVT = LdNode->getMemoryVT();

//   return Op;
// }

// Lower a f128 store into two f64 stores.
// static SDValue LowerF128Store(SDValue Op, SelectionDAG &DAG) {
//   // TODO: remove function
//   assert(false);
//   SDLoc dl(Op);
//   StoreSDNode *StNode = cast<StoreSDNode>(Op.getNode());
//   assert(StNode->getOffset().isUndef() && "Unexpected node type");

//   SDValue SubRegEven = DAG.getTargetConstant(SIM::sub_even64, dl, MVT::i32);
//   SDValue SubRegOdd  = DAG.getTargetConstant(SIM::sub_odd64, dl, MVT::i32);

//   SDNode *Hi64 = DAG.getMachineNode(TargetOpcode::EXTRACT_SUBREG,
//                                     dl,
//                                     MVT::f64,
//                                     StNode->getValue(),
//                                     SubRegEven);
//   SDNode *Lo64 = DAG.getMachineNode(TargetOpcode::EXTRACT_SUBREG,
//                                     dl,
//                                     MVT::f64,
//                                     StNode->getValue(),
//                                     SubRegOdd);

//   Align Alignment = commonAlignment(StNode->getOriginalAlign(), 8);

//   SDValue OutChains[2];
//   OutChains[0] =
//       DAG.getStore(StNode->getChain(), dl, SDValue(Hi64, 0),
//                    StNode->getBasePtr(), StNode->getPointerInfo(),
//                    Alignment);
//   EVT addrVT = StNode->getBasePtr().getValueType();
//   SDValue LoPtr = DAG.getNode(ISD::ADD, dl, addrVT,
//                               StNode->getBasePtr(),
//                               DAG.getConstant(8, dl, addrVT));
//   OutChains[1] = DAG.getStore(StNode->getChain(), dl, SDValue(Lo64, 0), LoPtr,
//                               StNode->getPointerInfo().getWithOffset(8),
//                               Alignment);
//   return DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains);
// }

// static SDValue LowerSTORE(SDValue Op, SelectionDAG &DAG)
// {
//   SDLoc dl(Op);
//   StoreSDNode *St = cast<StoreSDNode>(Op.getNode());

//   EVT MemVT = St->getMemoryVT();
//   if (MemVT == MVT::f128)
//     return LowerF128Store(Op, DAG);

//   if (MemVT == MVT::i64) {
//     // Custom handling for i64 stores: turn it into a bitcast and a
//     // v2i32 store.
//     SDValue Val = DAG.getNode(ISD::BITCAST, dl, MVT::v2i32, St->getValue());
//     SDValue Chain = DAG.getStore(
//         St->getChain(), dl, Val, St->getBasePtr(), St->getPointerInfo(),
//         St->getOriginalAlign(), St->getMemOperand()->getFlags(),
//         St->getAAInfo());
//     return Chain;
//   }

//   return SDValue();
// }

// static SDValue LowerFNEGorFABS(SDValue Op, SelectionDAG &DAG) {
//   assert((Op.getOpcode() == ISD::FNEG || Op.getOpcode() == ISD::FABS)
//          && "invalid opcode");

//   SDLoc dl(Op);

//   assert(Op.getValueType() == MVT::f32 && "Support only f32");
//   if (Op.getValueType() == MVT::f64)
//     return LowerF64Op(Op.getOperand(0), dl, DAG, Op.getOpcode());
//   if (Op.getValueType() != MVT::f128)
//     return Op;

//   // Lower fabs/fneg on f128 to fabs/fneg on f64
//   // fabs/fneg f128 => fabs/fneg f64:sub_even64, fmov f64:sub_odd64
//   // (As with LowerF64Op, on little-endian, we need to negate the odd
//   // subreg)

//   SDValue SrcReg128 = Op.getOperand(0);
//   SDValue Hi64 = DAG.getTargetExtractSubreg(SIM::sub_even64, dl, MVT::f64,
//                                             SrcReg128);
//   SDValue Lo64 = DAG.getTargetExtractSubreg(SIM::sub_odd64, dl, MVT::f64,
//                                             SrcReg128);

//   if (DAG.getDataLayout().isLittleEndian()) {
//     Lo64 = LowerF64Op(Lo64, dl, DAG, Op.getOpcode());
//   } else {
//     Hi64 = LowerF64Op(Hi64, dl, DAG, Op.getOpcode());
//   }

//   SDValue DstReg128 = SDValue(DAG.getMachineNode(TargetOpcode::IMPLICIT_DEF,
//                                                  dl, MVT::f128), 0);
//   DstReg128 = DAG.getTargetInsertSubreg(SIM::sub_even64, dl, MVT::f128,
//                                         DstReg128, Hi64);
//   DstReg128 = DAG.getTargetInsertSubreg(SIM::sub_odd64, dl, MVT::f128,
//                                         DstReg128, Lo64);
//   return DstReg128;
// }

// static SDValue LowerADDC_ADDE_SUBC_SUBE(SDValue Op, SelectionDAG &DAG) {

//   if (Op.getValueType() != MVT::i64)
//     return Op;

//   SDLoc dl(Op);
//   SDValue Src1 = Op.getOperand(0);
//   SDValue Src1Lo = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, Src1);
//   SDValue Src1Hi = DAG.getNode(ISD::SRL, dl, MVT::i64, Src1,
//                                DAG.getConstant(32, dl, MVT::i64));
//   Src1Hi = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, Src1Hi);

//   SDValue Src2 = Op.getOperand(1);
//   SDValue Src2Lo = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, Src2);
//   SDValue Src2Hi = DAG.getNode(ISD::SRL, dl, MVT::i64, Src2,
//                                DAG.getConstant(32, dl, MVT::i64));
//   Src2Hi = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, Src2Hi);


//   bool hasChain = false;
//   unsigned hiOpc = Op.getOpcode();
//   switch (Op.getOpcode()) {
//   default: llvm_unreachable("Invalid opcode");
//   case ISD::ADDC: hiOpc = ISD::ADDE; break;
//   case ISD::ADDE: hasChain = true; break;
//   case ISD::SUBC: hiOpc = ISD::SUBE; break;
//   case ISD::SUBE: hasChain = true; break;
//   }
//   SDValue Lo;
//   SDVTList VTs = DAG.getVTList(MVT::i32, MVT::Glue);
//   if (hasChain) {
//     Lo = DAG.getNode(Op.getOpcode(), dl, VTs, Src1Lo, Src2Lo,
//                      Op.getOperand(2));
//   } else {
//     Lo = DAG.getNode(Op.getOpcode(), dl, VTs, Src1Lo, Src2Lo);
//   }
//   SDValue Hi = DAG.getNode(hiOpc, dl, VTs, Src1Hi, Src2Hi, Lo.getValue(1));
//   SDValue Carry = Hi.getValue(1);

//   Lo = DAG.getNode(ISD::ZERO_EXTEND, dl, MVT::i64, Lo);
//   Hi = DAG.getNode(ISD::ZERO_EXTEND, dl, MVT::i64, Hi);
//   Hi = DAG.getNode(ISD::SHL, dl, MVT::i64, Hi,
//                    DAG.getConstant(32, dl, MVT::i64));

//   SDValue Dst = DAG.getNode(ISD::OR, dl, MVT::i64, Hi, Lo);
//   SDValue Ops[2] = { Dst, Carry };
//   return DAG.getMergeValues(Ops, dl);
// }

// Look at LHS/RHS/CC and see if they are a lowered setcc instruction.  If so
// set LHS/RHS and SPCC to the LHS/RHS of the setcc and SPCC to the condition.
// static void LookThroughSetCC(SDValue &LHS, SDValue &RHS,
//                              ISD::CondCode CC, unsigned &SPCC) {
//   if (isNullConstant(RHS) &&
//       CC == ISD::SETNE &&
//       (((LHS.getOpcode() == SPISD::SELECT_ICC ||
//          LHS.getOpcode() == SPISD::SELECT_XCC) &&
//         LHS.getOperand(3).getOpcode() == SPISD::CMPICC) ||
//        (LHS.getOpcode() == SPISD::SELECT_FCC &&
//         LHS.getOperand(3).getOpcode() == SPISD::CMPFCC)) &&
//       isOneConstant(LHS.getOperand(0)) &&
//       isNullConstant(LHS.getOperand(1))) {
//     SDValue CMPCC = LHS.getOperand(3);
//     SPCC = cast<ConstantSDNode>(LHS.getOperand(2))->getZExtValue();
//     LHS = CMPCC.getOperand(0);
//     RHS = CMPCC.getOperand(1);
//   }
// }

static SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG,
                          const SimTargetLowering &TLI) {
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);
  SDLoc dl(Op);
  unsigned Opc = ~0U;

  // If this is a br_cc of a "setcc", and if the setcc got lowered into
  // an CMP[IF]CC/SELECT_[IF]CC pair, find the original compared values.
  // LookThroughSetCC(LHS, RHS, CC, SPCC);

  if (CC == ISD::CondCode::SETGE) {
    CC = ISD::getSetCCSwappedOperands(CC);
    std::swap(LHS, RHS);
  }

  SDValue TargetCC = DAG.getCondCode(CC);
  return DAG.getNode(Opc, dl, Op.getValueType(), Op.getOperand(0),
                     LHS, RHS, TargetCC, Dest);
}

// static SDValue LowerSELECT_CC(SDValue Op, SelectionDAG &DAG,
//                               const SparcTargetLowering &TLI) {
//   SDValue LHS = Op.getOperand(0);
//   SDValue RHS = Op.getOperand(1);
//   ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();
//   SDValue TrueVal = Op.getOperand(2);
//   SDValue FalseVal = Op.getOperand(3);
//   SDLoc dl(Op);
//   unsigned Opc, SPCC = ~0U;

//   // If this is a select_cc of a "setcc", and if the setcc got lowered into
//   // an CMP[IF]CC/SELECT_[IF]CC pair, find the original compared values.
//   LookThroughSetCC(LHS, RHS, CC, SPCC);

//   SDValue CompareFlag;
//   if (LHS.getValueType().isInteger()) {
//     CompareFlag = DAG.getNode(SPISD::CMPICC, dl, MVT::Glue, LHS, RHS);
//     Opc = LHS.getValueType() == MVT::i32 ?
//           SPISD::SELECT_ICC : SPISD::SELECT_XCC;
//     if (SPCC == ~0U) SPCC = IntCondCCodeToICC(CC);
//   } else {
//     if (LHS.getValueType() == MVT::f128) {
//       if (SPCC == ~0U) SPCC = FPCondCCodeToFCC(CC);
//       CompareFlag = TLI.LowerF128Compare(LHS, RHS, SPCC, dl, DAG);
//       Opc = SPISD::SELECT_ICC;
//     } else {
//       CompareFlag = DAG.getNode(SPISD::CMPFCC, dl, MVT::Glue, LHS, RHS);
//       Opc = SPISD::SELECT_FCC;
//       if (SPCC == ~0U) SPCC = FPCondCCodeToFCC(CC);
//     }
//   }
//   return DAG.getNode(Opc, dl, TrueVal.getValueType(), TrueVal, FalseVal,
//                      DAG.getConstant(SPCC, dl, MVT::i32), CompareFlag);
// }

SDValue SimTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Should not custom lower this!");

  // case ISD::RETURNADDR:         return LowerRETURNADDR(Op, DAG, *this,
  //                                                      Subtarget);
  case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG,
                                                      Subtarget);
  // case ISD::FP_TO_SINT:         return LowerFP_TO_SINT(Op, DAG, *this);
  // case ISD::SINT_TO_FP:         return LowerSINT_TO_FP(Op, DAG, *this);
  // case ISD::FP_TO_UINT:         return LowerFP_TO_UINT(Op, DAG, *this);
  // case ISD::UINT_TO_FP:         return LowerUINT_TO_FP(Op, DAG, *this);
  case ISD::BR_CC:              return LowerBR_CC(Op, DAG, *this);
  // case ISD::SELECT_CC:          return LowerSELECT_CC(Op, DAG, *this);
  // case ISD::VAARG:              return LowerVAARG(Op, DAG);
  // case ISD::LOAD:               return LowerLOAD(Op, DAG);
  // case ISD::STORE:              return LowerSTORE(Op, DAG);
  // case ISD::FADD:               return LowerF128Op(Op, DAG,
  //                                      getLibcallName(RTLIB::ADD_F128), 2);
  // case ISD::FSUB:               return LowerF128Op(Op, DAG,
  //                                      getLibcallName(RTLIB::SUB_F128), 2);
  // case ISD::FMUL:               return LowerF128Op(Op, DAG,
  //                                      getLibcallName(RTLIB::MUL_F128), 2);
  // case ISD::FDIV:               return LowerF128Op(Op, DAG,
  //                                      getLibcallName(RTLIB::DIV_F128), 2);
  // case ISD::FSQRT:              return LowerF128Op(Op, DAG,
  //                                      getLibcallName(RTLIB::SQRT_F128),1);
  // case ISD::FABS:
  // case ISD::FNEG:               return LowerFNEGorFABS(Op, DAG, false);
  // case ISD::ADDC:
  // case ISD::ADDE:
  // case ISD::SUBC:
  // case ISD::SUBE:               return LowerADDC_ADDE_SUBC_SUBE(Op, DAG);
  }
}

// SDValue SimTargetLowering::bitcastConstantFPToInt(ConstantFPSDNode *C,
//                                                     const SDLoc &DL,
//                                                     SelectionDAG &DAG) const {
//   APInt V = C->getValueAPF().bitcastToAPInt();
//   SDValue Lo = DAG.getConstant(V.zextOrTrunc(32), DL, MVT::i32);
//   SDValue Hi = DAG.getConstant(V.lshr(32).zextOrTrunc(32), DL, MVT::i32);
//   if (DAG.getDataLayout().isLittleEndian())
//     std::swap(Lo, Hi);
//   return DAG.getBuildVector(MVT::v2i32, DL, {Hi, Lo});
// }

// SDValue SimTargetLowering::PerformBITCASTCombine(SDNode *N,
//                                                    DAGCombinerInfo &DCI) const {
//   SDLoc dl(N);
//   SDValue Src = N->getOperand(0);

//   if (isa<ConstantFPSDNode>(Src) && N->getSimpleValueType(0) == MVT::v2i32 &&
//       Src.getSimpleValueType() == MVT::f64)
//     return bitcastConstantFPToInt(cast<ConstantFPSDNode>(Src), dl, DCI.DAG);

//   return SDValue();
// }

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

//===----------------------------------------------------------------------===//
//                         Sim Inline Assembly Support
//===----------------------------------------------------------------------===//

/// getConstraintType - Given a constraint letter, return the type of
/// constraint it is for this target.
// SimTargetLowering::ConstraintType
// SimTargetLowering::getConstraintType(StringRef Constraint) const {
//   if (Constraint.size() == 1) {
//     switch (Constraint[0]) {
//     default:  break;
//     case 'r':
//     case 'f':
//     case 'e':
//       return C_RegisterClass;
//     case 'I': // SIMM13
//       return C_Immediate;
//     }
//   }

//   return TargetLowering::getConstraintType(Constraint);
// }

/// LowerAsmOperandForConstraint - Lower the specified operand into the Ops
/// vector.  If it is invalid, don't add anything to Ops.
// void SimTargetLowering::
// LowerAsmOperandForConstraint(SDValue Op,
//                              std::string &Constraint,
//                              std::vector<SDValue> &Ops,
//                              SelectionDAG &DAG) const {
//   SDValue Result(nullptr, 0);

//   // Only support length 1 constraints for now.
//   if (Constraint.length() > 1)
//     return;

//   char ConstraintLetter = Constraint[0];
//   switch (ConstraintLetter) {
//   default: break;
//   case 'I':
//     if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
//       if (isInt<13>(C->getSExtValue())) {
//         Result = DAG.getTargetConstant(C->getSExtValue(), SDLoc(Op),
//                                        Op.getValueType());
//         break;
//       }
//       return;
//     }
//   }

//   if (Result.getNode()) {
//     Ops.push_back(Result);
//     return;
//   }
//   TargetLowering::LowerAsmOperandForConstraint(Op, Constraint, Ops, DAG);
// }

void SimTargetLowering::ReplaceNodeResults(SDNode *N,
                                             SmallVectorImpl<SDValue>& Results,
                                             SelectionDAG &DAG) const {
  llvm_unreachable("TBD");
  // SDLoc dl(N);

  // RTLIB::Libcall libCall = RTLIB::UNKNOWN_LIBCALL;

  // switch (N->getOpcode()) {
  // default:
  //   llvm_unreachable("Do not know how to custom type legalize this operation!");

  // case ISD::FP_TO_SINT:
  // case ISD::FP_TO_UINT:
  //   // Custom lower only if it involves f128 or i64.
  //   if (N->getOperand(0).getValueType() != MVT::f128
  //       || N->getValueType(0) != MVT::i64)
  //     return;
  //   libCall = ((N->getOpcode() == ISD::FP_TO_SINT)
  //              ? RTLIB::FPTOSINT_F128_I64
  //              : RTLIB::FPTOUINT_F128_I64);

  //   Results.push_back(LowerF128Op(SDValue(N, 0),
  //                                 DAG,
  //                                 getLibcallName(libCall),
  //                                 1));
  //   return;
  // case ISD::READCYCLECOUNTER: {
  //   SDValue Lo = DAG.getCopyFromReg(N->getOperand(0), dl, SIM::ASR23, MVT::i32);
  //   SDValue Hi = DAG.getCopyFromReg(Lo, dl, SIM::G0, MVT::i32);
  //   SDValue Ops[] = { Lo, Hi };
  //   SDValue Pair = DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, Ops);
  //   Results.push_back(Pair);
  //   Results.push_back(N->getOperand(0));
  //   return;
  // }
  // case ISD::SINT_TO_FP:
  // case ISD::UINT_TO_FP:
  //   // Custom lower only if it involves f128 or i64.
  //   if (N->getValueType(0) != MVT::f128
  //       || N->getOperand(0).getValueType() != MVT::i64)
  //     return;

  //   libCall = ((N->getOpcode() == ISD::SINT_TO_FP)
  //              ? RTLIB::SINTTOFP_I64_F128
  //              : RTLIB::UINTTOFP_I64_F128);

  //   Results.push_back(LowerF128Op(SDValue(N, 0),
  //                                 DAG,
  //                                 getLibcallName(libCall),
  //                                 1));
  //   return;
  // case ISD::LOAD: {
  //   LoadSDNode *Ld = cast<LoadSDNode>(N);
  //   // Custom handling only for i64: turn i64 load into a v2i32 load,
  //   // and a bitcast.
  //   if (Ld->getValueType(0) != MVT::i64 || Ld->getMemoryVT() != MVT::i64)
  //     return;

  //   SDLoc dl(N);
  //   SDValue LoadRes = DAG.getExtLoad(
  //       Ld->getExtensionType(), dl, MVT::v2i32, Ld->getChain(),
  //       Ld->getBasePtr(), Ld->getPointerInfo(), MVT::v2i32,
  //       Ld->getOriginalAlign(), Ld->getMemOperand()->getFlags(),
  //       Ld->getAAInfo());

  //   SDValue Res = DAG.getNode(ISD::BITCAST, dl, MVT::i64, LoadRes);
  //   Results.push_back(Res);
  //   Results.push_back(LoadRes.getValue(1));
  //   return;
  // }
  // }
}
