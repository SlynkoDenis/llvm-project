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
// #include "MCTargetDesc/SimMCExpr.h"
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

    if (VA.needsCustom()) {
      assert(VA.getLocVT() == MVT::v2i32);
      // Legalize ret v2i32 -> ret 2 x i32 (Basically: do what would
      // happen by default if this wasn't a legal type)

      SDValue Part0 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, DL, MVT::i32,
                                  Arg,
                                  DAG.getConstant(0, DL, getVectorIdxTy(DAG.getDataLayout())));
      SDValue Part1 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, DL, MVT::i32,
                                  Arg,
                                  DAG.getConstant(1, DL, getVectorIdxTy(DAG.getDataLayout())));

      Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Part0, Flag);
      Flag = Chain.getValue(1);
      RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
      VA = RVLocs[++i]; // skip ahead to next loc
      Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Part1,
                               Flag);
    } else {
      Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Arg, Flag);
    }

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  unsigned RetAddrOffset = 8; // Call Inst + Delay Slot
  // If the function returns a struct, copy the SRetReturnReg to I0
  if (MF.getFunction().hasStructRetAttr()) {
    SimMachineFunctionInfo *SFI = MF.getInfo<SimMachineFunctionInfo>();
    Register Reg = SFI->getSRetReturnReg();
    if (!Reg) {
      llvm_unreachable("sret virtual register not created in the entry block");
    }
    auto PtrVT = getPointerTy(DAG.getDataLayout());
    SDValue Val = DAG.getCopyFromReg(Chain, DL, Reg, PtrVT);
    // TODO: change reg
    Chain = DAG.getCopyToReg(Chain, DL, SIM::R15, Val, Flag);
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(SIM::R14, PtrVT));
    RetAddrOffset = 12; // CallInst + Delay Slot + Unimp
  }

  RetOps[0] = Chain;  // Update chain.
  RetOps[1] = DAG.getConstant(RetAddrOffset, DL, MVT::i32);

  // Add the flag if we have it.
  if (Flag.getNode()) {
    RetOps.push_back(Flag);
  }

  return DAG.getNode(SIMISD::RET_FLAG, DL, MVT::Other, RetOps);
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
  bool IsLittleEndian = DAG.getDataLayout().isLittleEndian();

  unsigned InIdx = 0;
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i, ++InIdx) {
    CCValAssign &VA = ArgLocs[i];

    if (Ins[InIdx].Flags.isSRet()) {
      if (InIdx != 0)
        report_fatal_error("Sim only supports sret on the first parameter");
      // Get SRet from [%fp+64].
      int FrameIdx = MF.getFrameInfo().CreateFixedObject(4, 64, true);
      SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);
      SDValue Arg =
          DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
      InVals.push_back(Arg);
      continue;
    }

    if (VA.isRegLoc()) {
      if (VA.needsCustom()) {
        assert(VA.getLocVT() == MVT::f64 || VA.getLocVT() == MVT::v2i32);

        Register VRegHi = RegInfo.createVirtualRegister(&SIM::SimGPRReg);
        MF.getRegInfo().addLiveIn(VA.getLocReg(), VRegHi);
        SDValue HiVal = DAG.getCopyFromReg(Chain, dl, VRegHi, MVT::i32);

        assert(i+1 < e);
        CCValAssign &NextVA = ArgLocs[++i];

        SDValue LoVal;
        if (NextVA.isMemLoc()) {
          int FrameIdx = MF.getFrameInfo().
            CreateFixedObject(4, StackOffset+NextVA.getLocMemOffset(),true);
          SDValue FIPtr = DAG.getFrameIndex(FrameIdx, MVT::i32);
          LoVal = DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
        } else {
          Register loReg = MF.addLiveIn(NextVA.getLocReg(),
                                        &SIM::SimGPRReg);
          LoVal = DAG.getCopyFromReg(Chain, dl, loReg, MVT::i32);
        }

        if (IsLittleEndian) {
          std::swap(LoVal, HiVal);
        }

        SDValue WholeValue =
          DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, LoVal, HiVal);
        WholeValue = DAG.getNode(ISD::BITCAST, dl, VA.getLocVT(), WholeValue);
        InVals.push_back(WholeValue);
        continue;
      }
      Register VReg = RegInfo.createVirtualRegister(&SIM::SimGPRReg);
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

    unsigned Offset = VA.getLocMemOffset()+StackOffset;
    auto PtrVT = getPointerTy(DAG.getDataLayout());

    if (VA.needsCustom()) {
      assert(VA.getValVT() == MVT::f64 || VA.getValVT() == MVT::v2i32);
      // If it is double-word aligned, just load.
      if (Offset % 8 == 0) {
        int FI = MF.getFrameInfo().CreateFixedObject(8,
                                                     Offset,
                                                     true);
        SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
        SDValue Load =
            DAG.getLoad(VA.getValVT(), dl, Chain, FIPtr, MachinePointerInfo());
        InVals.push_back(Load);
        continue;
      }

      int FI = MF.getFrameInfo().CreateFixedObject(4,
                                                   Offset,
                                                   true);
      SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
      SDValue HiVal =
          DAG.getLoad(MVT::i32, dl, Chain, FIPtr, MachinePointerInfo());
      int FI2 = MF.getFrameInfo().CreateFixedObject(4,
                                                    Offset+4,
                                                    true);
      SDValue FIPtr2 = DAG.getFrameIndex(FI2, PtrVT);

      SDValue LoVal =
          DAG.getLoad(MVT::i32, dl, Chain, FIPtr2, MachinePointerInfo());

      if (IsLittleEndian) {
        std::swap(LoVal, HiVal);
      }

      SDValue WholeValue =
        DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, LoVal, HiVal);
      WholeValue = DAG.getNode(ISD::BITCAST, dl, VA.getValVT(), WholeValue);
      InVals.push_back(WholeValue);
      continue;
    }

    int FI = MF.getFrameInfo().CreateFixedObject(4,
                                                 Offset,
                                                 true);
    SDValue FIPtr = DAG.getFrameIndex(FI, PtrVT);
    SDValue Load ;
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
      Reg = MF.getRegInfo().createVirtualRegister(&SIM::SimGPRReg);
      SFI->setSRetReturnReg(Reg);
    }
    SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), dl, Reg, InVals[0]);
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, Copy, Chain);
  }

  // Store remaining ArgRegs to the stack if this is a varargs function.
  if (isVarArg) {
    static const MCPhysReg ArgRegs[] = {
      SIM::R10, SIM::R11, SIM::R12, SIM::R13, SIM::R14, SIM::R15
    };
    unsigned NumAllocated = CCInfo.getFirstUnallocated(ArgRegs);
    const MCPhysReg *CurArgReg = ArgRegs+NumAllocated, *ArgRegEnd = ArgRegs+6;
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
      Register VReg = RegInfo.createVirtualRegister(&SIM::SimGPRReg);
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

static bool hasReturnsTwiceAttr(SelectionDAG &DAG, SDValue Callee,
                                const CallBase *Call) {
  if (Call) {
    return Call->hasFnAttr(Attribute::ReturnsTwice);
  }

  const Function *CalleeFn = nullptr;
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    CalleeFn = dyn_cast<Function>(G->getGlobal());
  } else if (ExternalSymbolSDNode *E =
             dyn_cast<ExternalSymbolSDNode>(Callee)) {
    const Function &Fn = DAG.getMachineFunction().getFunction();
    const Module *M = Fn.getParent();
    const char *CalleeName = E->getSymbol();
    CalleeFn = M->getFunction(CalleeName);
  }

  if (!CalleeFn) {
    return false;
  }
  return CalleeFn->hasFnAttribute(Attribute::ReturnsTwice);
}

// Lower a call for the 32-bit ABI.
SDValue
SimTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                  SmallVectorImpl<SDValue> &InVals) const {
  llvm_unreachable("TBD");
  return SDValue();
  // SelectionDAG &DAG                     = CLI.DAG;
  // SDLoc &dl                             = CLI.DL;
  // SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  // SmallVectorImpl<SDValue> &OutVals     = CLI.OutVals;
  // SmallVectorImpl<ISD::InputArg> &Ins   = CLI.Ins;
  // SDValue Chain                         = CLI.Chain;
  // SDValue Callee                        = CLI.Callee;
  // bool &isTailCall                      = CLI.IsTailCall;
  // CallingConv::ID CallConv              = CLI.CallConv;
  // bool isVarArg                         = CLI.IsVarArg;

  // // Sim target does not yet support tail call optimization.
  // isTailCall = false;

  // // Analyze operands of the call, assigning locations to each operand.
  // SmallVector<CCValAssign, 16> ArgLocs;
  // CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
  //                *DAG.getContext());
  // CCInfo.AnalyzeCallOperands(Outs, CC_Sim);

  // // Get the size of the outgoing arguments stack space requirement.
  // unsigned ArgsSize = CCInfo.getNextStackOffset();

  // // Keep stack frames 8-byte aligned.
  // ArgsSize = (ArgsSize + 7) & ~7;

  // MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();

  // // Create local copies for byval args.
  // SmallVector<SDValue, 8> ByValArgs;
  // for (unsigned i = 0,  e = Outs.size(); i != e; ++i) {
  //   ISD::ArgFlagsTy Flags = Outs[i].Flags;
  //   if (!Flags.isByVal())
  //     continue;

  //   SDValue Arg = OutVals[i];
  //   unsigned Size = Flags.getByValSize();
  //   Align Alignment = Flags.getNonZeroByValAlign();

  //   if (Size > 0U) {
  //     int FI = MFI.CreateStackObject(Size, Alignment, false);
  //     SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
  //     SDValue SizeNode = DAG.getConstant(Size, dl, MVT::i32);

  //     Chain = DAG.getMemcpy(Chain, dl, FIPtr, Arg, SizeNode, Alignment,
  //                           false,        // isVolatile,
  //                           (Size <= 32), // AlwaysInline if size <= 32,
  //                           false,        // isTailCall
  //                           MachinePointerInfo(), MachinePointerInfo());
  //     ByValArgs.push_back(FIPtr);
  //   }
  //   else {
  //     SDValue nullVal;
  //     ByValArgs.push_back(nullVal);
  //   }
  // }

  // Chain = DAG.getCALLSEQ_START(Chain, ArgsSize, 0, dl);

  // SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  // SmallVector<SDValue, 8> MemOpChains;

  // const unsigned StackOffset = 92;
  // bool hasStructRetAttr = false;
  // unsigned SRetArgSize = 0;
  // // Walk the register/memloc assignments, inserting copies/loads.
  // for (unsigned i = 0, realArgIdx = 0, byvalArgIdx = 0, e = ArgLocs.size();
  //      i != e;
  //      ++i, ++realArgIdx) {
  //   CCValAssign &VA = ArgLocs[i];
  //   SDValue Arg = OutVals[realArgIdx];

  //   ISD::ArgFlagsTy Flags = Outs[realArgIdx].Flags;

  //   // Use local copy if it is a byval arg.
  //   if (Flags.isByVal()) {
  //     Arg = ByValArgs[byvalArgIdx++];
  //     if (!Arg) {
  //       continue;
  //     }
  //   }

  //   // Promote the value if needed.
  //   switch (VA.getLocInfo()) {
  //   default: llvm_unreachable("Unknown loc info!");
  //   case CCValAssign::Full: break;
  //   case CCValAssign::SExt:
  //     Arg = DAG.getNode(ISD::SIGN_EXTEND, dl, VA.getLocVT(), Arg);
  //     break;
  //   case CCValAssign::ZExt:
  //     Arg = DAG.getNode(ISD::ZERO_EXTEND, dl, VA.getLocVT(), Arg);
  //     break;
  //   case CCValAssign::AExt:
  //     Arg = DAG.getNode(ISD::ANY_EXTEND, dl, VA.getLocVT(), Arg);
  //     break;
  //   case CCValAssign::BCvt:
  //     Arg = DAG.getNode(ISD::BITCAST, dl, VA.getLocVT(), Arg);
  //     break;
  //   }

  //   if (Flags.isSRet()) {
  //     assert(VA.needsCustom());
  //     // store SRet argument in %sp+64
  //     SDValue StackPtr = DAG.getRegister(SIM::R15, MVT::i32);
  //     SDValue PtrOff = DAG.getIntPtrConstant(64, dl);
  //     PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
  //     MemOpChains.push_back(
  //         DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
  //     hasStructRetAttr = true;
  //     // sret only allowed on first argument
  //     assert(Outs[realArgIdx].OrigArgIndex == 0);
  //     PointerType *Ty = cast<PointerType>(CLI.getArgs()[0].Ty);
  //     Type *ElementTy = Ty->getElementType();
  //     SRetArgSize = DAG.getDataLayout().getTypeAllocSize(ElementTy);
  //     continue;
  //   }

  //   if (VA.needsCustom()) {
  //     assert(VA.getLocVT() == MVT::f64 || VA.getLocVT() == MVT::v2i32);

  //     if (VA.isMemLoc()) {
  //       unsigned Offset = VA.getLocMemOffset() + StackOffset;
  //       // if it is double-word aligned, just store.
  //       if (Offset % 8 == 0) {
  //         SDValue StackPtr = DAG.getRegister(SIM::R15, MVT::i32);
  //         SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
  //         PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
  //         MemOpChains.push_back(
  //             DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
  //         continue;
  //       }
  //     }

  //     if (VA.getLocVT() == MVT::f64) {
  //       // Move from the float value from float registers into the
  //       // integer registers.
  //       if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(Arg))
  //         Arg = bitcastConstantFPToInt(C, dl, DAG);
  //       else
  //         Arg = DAG.getNode(ISD::BITCAST, dl, MVT::v2i32, Arg);
  //     }

  //     SDValue Part0 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, MVT::i32,
  //                                 Arg,
  //                                 DAG.getConstant(0, dl, getVectorIdxTy(DAG.getDataLayout())));
  //     SDValue Part1 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, dl, MVT::i32,
  //                                 Arg,
  //                                 DAG.getConstant(1, dl, getVectorIdxTy(DAG.getDataLayout())));

  //     if (VA.isRegLoc()) {
  //       RegsToPass.push_back(std::make_pair(VA.getLocReg(), Part0));
  //       assert(i+1 != e);
  //       CCValAssign &NextVA = ArgLocs[++i];
  //       if (NextVA.isRegLoc()) {
  //         RegsToPass.push_back(std::make_pair(NextVA.getLocReg(), Part1));
  //       } else {
  //         // Store the second part in stack.
  //         unsigned Offset = NextVA.getLocMemOffset() + StackOffset;
  //         SDValue StackPtr = DAG.getRegister(SIM::R15, MVT::i32);
  //         SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
  //         PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
  //         MemOpChains.push_back(
  //             DAG.getStore(Chain, dl, Part1, PtrOff, MachinePointerInfo()));
  //       }
  //     } else {
  //       unsigned Offset = VA.getLocMemOffset() + StackOffset;
  //       // Store the first part.
  //       SDValue StackPtr = DAG.getRegister(SIM::R15, MVT::i32);
  //       SDValue PtrOff = DAG.getIntPtrConstant(Offset, dl);
  //       PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
  //       MemOpChains.push_back(
  //           DAG.getStore(Chain, dl, Part0, PtrOff, MachinePointerInfo()));
  //       // Store the second part.
  //       PtrOff = DAG.getIntPtrConstant(Offset + 4, dl);
  //       PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
  //       MemOpChains.push_back(
  //           DAG.getStore(Chain, dl, Part1, PtrOff, MachinePointerInfo()));
  //     }
  //     continue;
  //   }

  //   // Arguments that can be passed on register must be kept at
  //   // RegsToPass vector
  //   if (VA.isRegLoc()) {
  //     if (VA.getLocVT() != MVT::f32) {
  //       RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
  //       continue;
  //     }
  //     Arg = DAG.getNode(ISD::BITCAST, dl, MVT::i32, Arg);
  //     RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
  //     continue;
  //   }

  //   assert(VA.isMemLoc());

  //   // Create a store off the stack pointer for this argument.
  //   SDValue StackPtr = DAG.getRegister(SIM::R15, MVT::i32);
  //   SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset() + StackOffset,
  //                                          dl);
  //   PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);
  //   MemOpChains.push_back(
  //       DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo()));
  // }


  // // Emit all stores, make sure the occur before any copies into physregs.
  // if (!MemOpChains.empty())
  //   Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOpChains);

  // // Build a sequence of copy-to-reg nodes chained together with token
  // // chain and flag operands which copy the outgoing args into registers.
  // // The InFlag in necessary since all emitted instructions must be
  // // stuck together.
  // SDValue InFlag;
  // for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
  //   Register Reg = toCallerWindow(RegsToPass[i].first);
  //   Chain = DAG.getCopyToReg(Chain, dl, Reg, RegsToPass[i].second, InFlag);
  //   InFlag = Chain.getValue(1);
  // }

  // bool hasReturnsTwice = hasReturnsTwiceAttr(DAG, Callee, CLI.CB);

  // // If the callee is a GlobalAddress node (quite common, every direct call is)
  // // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // // Likewise ExternalSymbol -> TargetExternalSymbol.
  // if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee))
  //   Callee = DAG.getTargetGlobalAddress(G->getGlobal(), dl, MVT::i32, 0);
  // else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee))
  //   Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i32);

  // // Returns a chain & a flag for retval copy to use
  // SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  // SmallVector<SDValue, 8> Ops;
  // Ops.push_back(Chain);
  // Ops.push_back(Callee);
  // if (hasStructRetAttr)
  //   Ops.push_back(DAG.getTargetConstant(SRetArgSize, dl, MVT::i32));
  // for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i)
  //   Ops.push_back(DAG.getRegister(toCallerWindow(RegsToPass[i].first),
  //                                 RegsToPass[i].second.getValueType()));

  // // Add a register mask operand representing the call-preserved registers.
  // const SimRegisterInfo *TRI = Subtarget->getRegisterInfo();
  // const uint32_t *Mask =
  //     ((hasReturnsTwice)
  //          ? TRI->getRTCallPreservedMask(CallConv)
  //          : TRI->getCallPreservedMask(DAG.getMachineFunction(), CallConv));
  // assert(Mask && "Missing call preserved mask for calling convention");
  // Ops.push_back(DAG.getRegisterMask(Mask));

  // if (InFlag.getNode())
  //   Ops.push_back(InFlag);

  // Chain = DAG.getNode(SIMISD::CALL, dl, NodeTys, Ops);
  // InFlag = Chain.getValue(1);

  // Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(ArgsSize, dl, true),
  //                            DAG.getIntPtrConstant(0, dl, true), InFlag, dl);
  // InFlag = Chain.getValue(1);

  // // Assign locations to each value returned by this call.
  // SmallVector<CCValAssign, 16> RVLocs;
  // CCState RVInfo(CallConv, isVarArg, DAG.getMachineFunction(), RVLocs,
  //                *DAG.getContext());

  // RVInfo.AnalyzeCallResult(Ins, RetCC_Sim);

  // // Copy all of the result registers out of their specified physreg.
  // for (unsigned i = 0; i != RVLocs.size(); ++i) {
  //   if (RVLocs[i].getLocVT() == MVT::v2i32) {
  //     SDValue Vec = DAG.getNode(ISD::UNDEF, dl, MVT::v2i32);
  //     SDValue Lo = DAG.getCopyFromReg(
  //         Chain, dl, toCallerWindow(RVLocs[i++].getLocReg()), MVT::i32, InFlag);
  //     Chain = Lo.getValue(1);
  //     InFlag = Lo.getValue(2);
  //     Vec = DAG.getNode(ISD::INSERT_VECTOR_ELT, dl, MVT::v2i32, Vec, Lo,
  //                       DAG.getConstant(0, dl, MVT::i32));
  //     SDValue Hi = DAG.getCopyFromReg(
  //         Chain, dl, toCallerWindow(RVLocs[i].getLocReg()), MVT::i32, InFlag);
  //     Chain = Hi.getValue(1);
  //     InFlag = Hi.getValue(2);
  //     Vec = DAG.getNode(ISD::INSERT_VECTOR_ELT, dl, MVT::v2i32, Vec, Hi,
  //                       DAG.getConstant(1, dl, MVT::i32));
  //     InVals.push_back(Vec);
  //   } else {
  //     Chain =
  //         DAG.getCopyFromReg(Chain, dl, toCallerWindow(RVLocs[i].getLocReg()),
  //                            RVLocs[i].getValVT(), InFlag)
  //             .getValue(1);
  //     InFlag = Chain.getValue(2);
  //     InVals.push_back(Chain.getValue(0));
  //   }
  // }

  // return Chain;
}

// FIXME? Maybe this could be a TableGen attribute on some registers and
// this table could be generated automatically from RegInfo.
Register SimTargetLowering::getRegisterByName(const char* RegName, LLT VT,
                                                const MachineFunction &MF) const {
  // Register Reg = StringSwitch<Register>(RegName)
  //   .Case("i0", SIM::I0).Case("i1", SIM::I1).Case("i2", SIM::I2).Case("i3", SIM::I3)
  //   .Case("i4", SIM::I4).Case("i5", SIM::I5).Case("i6", SIM::I6).Case("i7", SIM::I7)
  //   .Case("o0", SIM::O0).Case("o1", SIM::O1).Case("o2", SIM::O2).Case("o3", SIM::O3)
  //   .Case("o4", SIM::O4).Case("o5", SIM::O5).Case("o6", SIM::O6).Case("o7", SIM::O7)
  //   .Case("l0", SIM::L0).Case("l1", SIM::L1).Case("l2", SIM::L2).Case("l3", SIM::L3)
  //   .Case("l4", SIM::L4).Case("l5", SIM::L5).Case("l6", SIM::L6).Case("l7", SIM::L7)
  //   .Case("g0", SIM::G0).Case("g1", SIM::G1).Case("g2", SIM::G2).Case("g3", SIM::G3)
  //   .Case("g4", SIM::G4).Case("g5", SIM::G5).Case("g6", SIM::G6).Case("g7", SIM::G7)
  //   .Default(0);
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

TargetLowering::AtomicExpansionKind SimTargetLowering::shouldExpandAtomicRMWInIR(AtomicRMWInst *) const {
  return AtomicExpansionKind::None;
}

SimTargetLowering::SimTargetLowering(const TargetMachine &TM,
                                         const SimSubtarget &STI)
    : TargetLowering(TM), Subtarget(&STI) {
  MVT PtrVT = MVT::getIntegerVT(8 * TM.getPointerSize(0));

  // Instructions which use registers as conditionals examine all the
  // bits (as does the pseudo SELECT_CC expansion). I don't think it
  // matters much whether it's ZeroOrOneBooleanContent, or
  // ZeroOrNegativeOneBooleanContent, so, arbitrarily choose the
  // former.
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);

  // Set up the register classes.
  addRegisterClass(MVT::i32, &SIM::SimGPRReg);
  if (!Subtarget->useSoftFloat()) {
    addRegisterClass(MVT::f32, &SIM::SimGPRReg);
    // addRegisterClass(MVT::f64, &SIM::DFPRegsRegClass);
    // addRegisterClass(MVT::f128, &SIM::QFPRegsRegClass);
  }
  // TODO: remove commented part
  // On 32bit Sim, we define a double-register 32bit register
  // class, as well. This is modeled in LLVM as a 2-vector of i32.
  // addRegisterClass(MVT::v2i32, &SIM::IntPairRegClass);
  // ...but almost all operations must be expanded, so set that as
  // the default.
  // for (unsigned Op = 0; Op < ISD::BUILTIN_OP_END; ++Op) {
  //   setOperationAction(Op, MVT::v2i32, Expand);
  // }
  // Truncating/extending stores/loads are also not supported.
  // for (MVT VT : MVT::integer_fixedlen_vector_valuetypes()) {
  //   setLoadExtAction(ISD::SEXTLOAD, VT, MVT::v2i32, Expand);
  //   setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::v2i32, Expand);
  //   setLoadExtAction(ISD::EXTLOAD, VT, MVT::v2i32, Expand);
  //   setLoadExtAction(ISD::SEXTLOAD, MVT::v2i32, VT, Expand);
  //   setLoadExtAction(ISD::ZEXTLOAD, MVT::v2i32, VT, Expand);
  //   setLoadExtAction(ISD::EXTLOAD, MVT::v2i32, VT, Expand);
  //   setTruncStoreAction(VT, MVT::v2i32, Expand);
  //   setTruncStoreAction(MVT::v2i32, VT, Expand);
  // }
  // // However, load and store *are* legal.
  // setOperationAction(ISD::LOAD, MVT::v2i32, Legal);
  // setOperationAction(ISD::STORE, MVT::v2i32, Legal);
  // setOperationAction(ISD::EXTRACT_VECTOR_ELT, MVT::v2i32, Legal);
  // setOperationAction(ISD::BUILD_VECTOR, MVT::v2i32, Legal);
  // TODO: uncomment
  // And we need to promote i64 loads/stores into vector load/store
  // setOperationAction(ISD::LOAD, MVT::i64, Custom);
  // setOperationAction(ISD::STORE, MVT::i64, Custom);

  // Turn FP extload into load/fpextend
  for (MVT VT : MVT::fp_valuetypes()) {
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f16, Expand);
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f32, Expand);
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f64, Expand);
  }

  // Sim doesn't have i1 sign extending load
  for (MVT VT : MVT::integer_valuetypes())
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i1, Promote);

  // Turn FP truncstore into trunc + store.
  // setTruncStoreAction(MVT::f32, MVT::f16, Expand);
  // setTruncStoreAction(MVT::f64, MVT::f16, Expand);
  // setTruncStoreAction(MVT::f64, MVT::f32, Expand);
  // setTruncStoreAction(MVT::f128, MVT::f16, Expand);
  // setTruncStoreAction(MVT::f128, MVT::f32, Expand);
  // setTruncStoreAction(MVT::f128, MVT::f64, Expand);

  // Custom legalize GlobalAddress nodes into LO/HI parts.
  setOperationAction(ISD::GlobalAddress, PtrVT, Custom);
  setOperationAction(ISD::GlobalTLSAddress, PtrVT, Custom);
  setOperationAction(ISD::ConstantPool, PtrVT, Custom);
  setOperationAction(ISD::BlockAddress, PtrVT, Custom);

  // Sim doesn't have sext_inreg, replace them with shl/sra
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1 , Expand);

  // Sim has no REM or DIVREM operations.
  setOperationAction(ISD::UREM, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);

  // Custom expand fp<->sint
  setOperationAction(ISD::FP_TO_SINT, MVT::i32, Custom);
  setOperationAction(ISD::SINT_TO_FP, MVT::i32, Custom);
  setOperationAction(ISD::FP_TO_SINT, MVT::i64, Custom);
  setOperationAction(ISD::SINT_TO_FP, MVT::i64, Custom);

  // Custom Expand fp<->uint
  setOperationAction(ISD::FP_TO_UINT, MVT::i32, Custom);
  setOperationAction(ISD::UINT_TO_FP, MVT::i32, Custom);
  setOperationAction(ISD::FP_TO_UINT, MVT::i64, Custom);
  setOperationAction(ISD::UINT_TO_FP, MVT::i64, Custom);

  // Lower f16 conversion operations into library calls
  setOperationAction(ISD::FP16_TO_FP, MVT::f32, Expand);
  setOperationAction(ISD::FP_TO_FP16, MVT::f32, Expand);
  setOperationAction(ISD::FP16_TO_FP, MVT::f64, Expand);
  setOperationAction(ISD::FP_TO_FP16, MVT::f64, Expand);
  setOperationAction(ISD::FP16_TO_FP, MVT::f128, Expand);
  setOperationAction(ISD::FP_TO_FP16, MVT::f128, Expand);

  setOperationAction(ISD::BITCAST, MVT::f32, Expand);
  setOperationAction(ISD::BITCAST, MVT::i32, Expand);

  // Sim has no select or setcc: expand to SELECT_CC.
  setOperationAction(ISD::SELECT, MVT::i32, Expand);
  setOperationAction(ISD::SELECT, MVT::f32, Expand);
  setOperationAction(ISD::SELECT, MVT::f64, Expand);
  setOperationAction(ISD::SELECT, MVT::f128, Expand);

  setOperationAction(ISD::SETCC, MVT::i32, Expand);
  setOperationAction(ISD::SETCC, MVT::f32, Expand);
  setOperationAction(ISD::SETCC, MVT::f64, Expand);
  setOperationAction(ISD::SETCC, MVT::f128, Expand);

  // Sim doesn't have BRCOND either, it has BR_CC.
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  setOperationAction(ISD::BRIND, MVT::Other, Expand);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC, MVT::i32, Custom);
  setOperationAction(ISD::BR_CC, MVT::f32, Custom);
  setOperationAction(ISD::BR_CC, MVT::f64, Custom);
  setOperationAction(ISD::BR_CC, MVT::f128, Custom);

  setOperationAction(ISD::SELECT_CC, MVT::i32, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f32, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f64, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f128, Custom);

  setOperationAction(ISD::ADDC, MVT::i32, Custom);
  setOperationAction(ISD::ADDE, MVT::i32, Custom);
  setOperationAction(ISD::SUBC, MVT::i32, Custom);
  setOperationAction(ISD::SUBE, MVT::i32, Custom);

  // Atomics are unsupported
  setMaxAtomicSizeInBitsSupported(0);

  setMinCmpXchgSizeInBits(32);

  setOperationAction(ISD::ATOMIC_SWAP, MVT::i32, Legal);

  setOperationAction(ISD::ATOMIC_FENCE, MVT::Other, Legal);

  // Custom Lower Atomic LOAD/STORE
  setOperationAction(ISD::ATOMIC_LOAD, MVT::i32, Custom);
  setOperationAction(ISD::ATOMIC_STORE, MVT::i32, Custom);

  // These libcalls are not available in 32-bit.
  setLibcallName(RTLIB::MULO_I64, nullptr);
  setLibcallName(RTLIB::SHL_I128, nullptr);
  setLibcallName(RTLIB::SRL_I128, nullptr);
  setLibcallName(RTLIB::SRA_I128, nullptr);

  setLibcallName(RTLIB::MULO_I128, nullptr);

  setOperationAction(ISD::FNEG, MVT::f64, Custom);
  setOperationAction(ISD::FABS, MVT::f64, Custom);

  setOperationAction(ISD::FSIN , MVT::f128, Expand);
  setOperationAction(ISD::FCOS , MVT::f128, Expand);
  setOperationAction(ISD::FSINCOS, MVT::f128, Expand);
  setOperationAction(ISD::FREM , MVT::f128, Expand);
  setOperationAction(ISD::FMA  , MVT::f128, Expand);
  setOperationAction(ISD::FSIN , MVT::f64, Expand);
  setOperationAction(ISD::FCOS , MVT::f64, Expand);
  setOperationAction(ISD::FSINCOS, MVT::f64, Expand);
  setOperationAction(ISD::FREM , MVT::f64, Expand);
  setOperationAction(ISD::FMA  , MVT::f64, Expand);
  setOperationAction(ISD::FSIN , MVT::f32, Expand);
  setOperationAction(ISD::FCOS , MVT::f32, Expand);
  setOperationAction(ISD::FSINCOS, MVT::f32, Expand);
  setOperationAction(ISD::FREM , MVT::f32, Expand);
  setOperationAction(ISD::FMA  , MVT::f32, Expand);
  setOperationAction(ISD::CTTZ , MVT::i32, Expand);
  setOperationAction(ISD::CTLZ , MVT::i32, Expand);
  setOperationAction(ISD::ROTL , MVT::i32, Expand);
  setOperationAction(ISD::ROTR , MVT::i32, Expand);
  setOperationAction(ISD::BSWAP, MVT::i32, Expand);
  setOperationAction(ISD::FCOPYSIGN, MVT::f128, Expand);
  setOperationAction(ISD::FCOPYSIGN, MVT::f64, Expand);
  setOperationAction(ISD::FCOPYSIGN, MVT::f32, Expand);
  setOperationAction(ISD::FPOW , MVT::f128, Expand);
  setOperationAction(ISD::FPOW , MVT::f64, Expand);
  setOperationAction(ISD::FPOW , MVT::f32, Expand);

  setOperationAction(ISD::SHL_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRA_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRL_PARTS, MVT::i32, Expand);

  // Expands to [SU]MUL_LOHI.
  setOperationAction(ISD::MULHU,     MVT::i32, Expand);
  setOperationAction(ISD::MULHS,     MVT::i32, Expand);
  setOperationAction(ISD::MUL,       MVT::i32, Expand);

  // TODO: change it
  if (Subtarget->useSoftMulDiv()) {
    // .umul works for both signed and unsigned
    setOperationAction(ISD::SMUL_LOHI, MVT::i32, Expand);
    setOperationAction(ISD::UMUL_LOHI, MVT::i32, Expand);
    setLibcallName(RTLIB::MUL_I32, ".umul");

    setOperationAction(ISD::SDIV, MVT::i32, Expand);
    setLibcallName(RTLIB::SDIV_I32, ".div");

    setOperationAction(ISD::UDIV, MVT::i32, Expand);
    setLibcallName(RTLIB::UDIV_I32, ".udiv");

    setLibcallName(RTLIB::SREM_I32, ".rem");
    setLibcallName(RTLIB::UREM_I32, ".urem");
  }

  // VASTART needs to be custom lowered to use the VarArgsFrameIndex.
  setOperationAction(ISD::VASTART           , MVT::Other, Custom);
  // VAARG needs to be lowered to not do unaligned accesses for doubles.
  setOperationAction(ISD::VAARG             , MVT::Other, Custom);

  setOperationAction(ISD::TRAP              , MVT::Other, Legal);
  setOperationAction(ISD::DEBUGTRAP         , MVT::Other, Legal);

  // Use the default implementation.
  setOperationAction(ISD::VACOPY            , MVT::Other, Expand);
  setOperationAction(ISD::VAEND             , MVT::Other, Expand);
  setOperationAction(ISD::STACKSAVE         , MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE      , MVT::Other, Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32  , Custom);

  setStackPointerRegisterToSaveRestore(SIM::R15);

  setOperationAction(ISD::CTPOP, MVT::i32, Expand);

  setOperationAction(ISD::LOAD, MVT::f128, Custom);
  setOperationAction(ISD::STORE, MVT::f128, Custom);

  // Custom legalize f128 operations.

  setOperationAction(ISD::FADD,  MVT::f128, Custom);
  setOperationAction(ISD::FSUB,  MVT::f128, Custom);
  setOperationAction(ISD::FMUL,  MVT::f128, Custom);
  setOperationAction(ISD::FDIV,  MVT::f128, Custom);
  setOperationAction(ISD::FSQRT, MVT::f128, Custom);
  setOperationAction(ISD::FNEG,  MVT::f128, Custom);
  setOperationAction(ISD::FABS,  MVT::f128, Custom);

  setOperationAction(ISD::FP_EXTEND, MVT::f128, Custom);
  setOperationAction(ISD::FP_ROUND,  MVT::f64, Custom);
  setOperationAction(ISD::FP_ROUND,  MVT::f32, Custom);

  // Setup Runtime library names.
  if (!Subtarget->useSoftFloat()) {
    setLibcallName(RTLIB::ADD_F128,  "_Q_add");
    setLibcallName(RTLIB::SUB_F128,  "_Q_sub");
    setLibcallName(RTLIB::MUL_F128,  "_Q_mul");
    setLibcallName(RTLIB::DIV_F128,  "_Q_div");
    setLibcallName(RTLIB::SQRT_F128, "_Q_sqrt");
    setLibcallName(RTLIB::FPTOSINT_F128_I32, "_Q_qtoi");
    setLibcallName(RTLIB::FPTOUINT_F128_I32, "_Q_qtou");
    setLibcallName(RTLIB::SINTTOFP_I32_F128, "_Q_itoq");
    setLibcallName(RTLIB::UINTTOFP_I32_F128, "_Q_utoq");
    setLibcallName(RTLIB::FPTOSINT_F128_I64, "_Q_qtoll");
    setLibcallName(RTLIB::FPTOUINT_F128_I64, "_Q_qtoull");
    setLibcallName(RTLIB::SINTTOFP_I64_F128, "_Q_lltoq");
    setLibcallName(RTLIB::UINTTOFP_I64_F128, "_Q_ulltoq");
    setLibcallName(RTLIB::FPEXT_F32_F128, "_Q_stoq");
    setLibcallName(RTLIB::FPEXT_F64_F128, "_Q_dtoq");
    setLibcallName(RTLIB::FPROUND_F128_F32, "_Q_qtos");
    setLibcallName(RTLIB::FPROUND_F128_F64, "_Q_qtod");
  }

  if (Subtarget->hasNoFMULS()) {
    setOperationAction(ISD::FMUL, MVT::f32, Promote);
  }

  setTargetDAGCombine(ISD::BITCAST);

  setOperationAction(ISD::INTRINSIC_WO_CHAIN, MVT::Other, Custom);

  setMinFunctionAlignment(Align(4));

  computeRegisterProperties(Subtarget->getRegisterInfo());
}

bool SimTargetLowering::useSoftFloat() const {
  return Subtarget->useSoftFloat();
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
  case SIMISD::RET_FLAG:        return "SIMISD::RET_FLAG";
  case SIMISD::GLOBAL_BASE_REG: return "SIMISD::GLOBAL_BASE_REG";
  case SIMISD::FLUSHW:          return "SIMISD::FLUSHW";
  case SIMISD::TLS_ADD:         return "SIMISD::TLS_ADD";
  case SIMISD::TLS_LD:          return "SIMISD::TLS_LD";
  case SIMISD::TLS_CALL:        return "SIMISD::TLS_CALL";
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

/// isMaskedValueZeroForTargetNode - Return true if 'Op & Mask' is known to
/// be zero. Op is expected to be a target specific node. Used by DAG
/// combiner.
void SimTargetLowering::computeKnownBitsForTargetNode
                                (const SDValue Op,
                                 KnownBits &Known,
                                 const APInt &DemandedElts,
                                 const SelectionDAG &DAG,
                                 unsigned Depth) const {
  // KnownBits Known2;
  Known.resetAll();

  // TODO: remove commented
  // switch (Op.getOpcode()) {
  // default: break;
  // case SIMISD::SELECT_ICC:
  // case SIMISD::SELECT_XCC:
  // case SIMISD::SELECT_FCC:
  //   Known = DAG.computeKnownBits(Op.getOperand(1), Depth + 1);
  //   Known2 = DAG.computeKnownBits(Op.getOperand(0), Depth + 1);

  //   // Only known if known in both the LHS and RHS.
  //   Known = KnownBits::commonBits(Known, Known2);
  //   break;
  // }
}

// TODO: remove
// // Look at LHS/RHS/CC and see if they are a lowered setcc instruction.  If so
// // set LHS/RHS and SPCC to the LHS/RHS of the setcc and SPCC to the condition.
// static void LookThroughSetCC(SDValue &LHS, SDValue &RHS,
//                              ISD::CondCode CC, unsigned &SPCC) {
//   if (isNullConstant(RHS) &&
//       CC == ISD::SETNE &&
//       (((LHS.getOpcode() == SIMISD::SELECT_ICC ||
//          LHS.getOpcode() == SIMISD::SELECT_XCC) &&
//         LHS.getOperand(3).getOpcode() == SIMISD::CMPICC) ||
//        (LHS.getOpcode() == SIMISD::SELECT_FCC &&
//         LHS.getOperand(3).getOpcode() == SIMISD::CMPFCC)) &&
//       isOneConstant(LHS.getOperand(0)) &&
//       isNullConstant(LHS.getOperand(1))) {
//     SDValue CMPCC = LHS.getOperand(3);
//     SPCC = cast<ConstantSDNode>(LHS.getOperand(2))->getZExtValue();
//     LHS = CMPCC.getOperand(0);
//     RHS = CMPCC.getOperand(1);
//   }
// }

// Convert to a target node and set target flags.
SDValue SimTargetLowering::withTargetFlags(SDValue Op, unsigned TF,
                                             SelectionDAG &DAG) const {
  if (const GlobalAddressSDNode *GA = dyn_cast<GlobalAddressSDNode>(Op))
    return DAG.getTargetGlobalAddress(GA->getGlobal(),
                                      SDLoc(GA),
                                      GA->getValueType(0),
                                      GA->getOffset(), TF);

  if (const ConstantPoolSDNode *CP = dyn_cast<ConstantPoolSDNode>(Op))
    return DAG.getTargetConstantPool(CP->getConstVal(), CP->getValueType(0),
                                     CP->getAlign(), CP->getOffset(), TF);

  if (const BlockAddressSDNode *BA = dyn_cast<BlockAddressSDNode>(Op))
    return DAG.getTargetBlockAddress(BA->getBlockAddress(),
                                     Op.getValueType(),
                                     0,
                                     TF);

  if (const ExternalSymbolSDNode *ES = dyn_cast<ExternalSymbolSDNode>(Op))
    return DAG.getTargetExternalSymbol(ES->getSymbol(),
                                       ES->getValueType(0), TF);

  llvm_unreachable("Unhandled address SDNode");
}

// Split Op into high and low parts according to HiTF and LoTF.
// Return an ADD node combining the parts.
SDValue SimTargetLowering::makeHiLoPair(SDValue Op,
                                        unsigned HiTF, unsigned LoTF,
                                        SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT VT = Op.getValueType();
  SDValue Hi = DAG.getNode(SIMISD::Hi, DL, VT, withTargetFlags(Op, HiTF, DAG));
  SDValue Lo = DAG.getNode(SIMISD::Lo, DL, VT, withTargetFlags(Op, LoTF, DAG));
  return DAG.getNode(ISD::ADD, DL, VT, Hi, Lo);
}

// Build SDNodes for producing an address from a GlobalAddress, ConstantPool,
// or ExternalSymbol SDNode.
SDValue SimTargetLowering::makeAddress(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT VT = getPointerTy(DAG.getDataLayout());

  // Handle PIC mode first. Sim needs a got load for every variable!
  if (isPositionIndependent()) {
    const Module *M = DAG.getMachineFunction().getFunction().getParent();
    PICLevel::Level picLevel = M->getPICLevel();
    SDValue Idx;

    if (picLevel == PICLevel::SmallPIC) {
      // This is the pic13 code model, the GOT is known to be smaller than 8KiB.
      Idx = DAG.getNode(SIMISD::Lo, DL, Op.getValueType(),
                        withTargetFlags(Op, 0, DAG));
    } else {
      // This is the pic32 code model, the GOT is known to be smaller than 4GB.
      Idx = makeHiLoPair(Op, SimMCExpr::VK_Sim_GOT22,
                         SimMCExpr::VK_Sim_GOT10, DAG);
    }

    SDValue GlobalBase = DAG.getNode(SIMISD::GLOBAL_BASE_REG, DL, VT);
    SDValue AbsAddr = DAG.getNode(ISD::ADD, DL, VT, GlobalBase, Idx);
    // GLOBAL_BASE_REG codegen'ed with call. Inform MFI that this
    // function has calls.
    MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
    MFI.setHasCalls(true);
    return DAG.getLoad(VT, DL, DAG.getEntryNode(), AbsAddr,
                       MachinePointerInfo::getGOT(DAG.getMachineFunction()));
  }

  // This is one of the absolute code models.
  switch(getTargetMachine().getCodeModel()) {
  default:
    llvm_unreachable("Unsupported absolute code model");
  case CodeModel::Small:
    // abs32.
    return makeHiLoPair(Op, SimMCExpr::VK_Sim_HI,
                        SimMCExpr::VK_Sim_LO, DAG);
  case CodeModel::Medium: {
    // abs44.
    SDValue H44 = makeHiLoPair(Op, SimMCExpr::VK_Sim_H44,
                               SimMCExpr::VK_Sim_M44, DAG);
    H44 = DAG.getNode(ISD::SHL, DL, VT, H44, DAG.getConstant(12, DL, MVT::i32));
    SDValue L44 = withTargetFlags(Op, 0, DAG);
    L44 = DAG.getNode(SIMISD::Lo, DL, VT, L44);
    return DAG.getNode(ISD::ADD, DL, VT, H44, L44);
  }
  case CodeModel::Large: {
    // abs64.
    SDValue Hi = makeHiLoPair(Op, SimMCExpr::VK_Sim_HH,
                              SimMCExpr::VK_Sim_HM, DAG);
    Hi = DAG.getNode(ISD::SHL, DL, VT, Hi, DAG.getConstant(32, DL, MVT::i32));
    SDValue Lo = makeHiLoPair(Op, SimMCExpr::VK_Sim_HI,
                              SimMCExpr::VK_Sim_LO, DAG);
    return DAG.getNode(ISD::ADD, DL, VT, Hi, Lo);
  }
  }
}

SDValue SimTargetLowering::LowerGlobalAddress(SDValue Op,
                                                SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue SimTargetLowering::LowerConstantPool(SDValue Op,
                                               SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue SimTargetLowering::LowerBlockAddress(SDValue Op,
                                               SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue SimTargetLowering::LowerGlobalTLSAddress(SDValue Op,
                                                   SelectionDAG &DAG) const {

  GlobalAddressSDNode *GA = cast<GlobalAddressSDNode>(Op);
  if (DAG.getTarget().useEmulatedTLS())
    return LowerToTLSEmulatedModel(GA, DAG);

  SDLoc DL(GA);
  const GlobalValue *GV = GA->getGlobal();
  EVT PtrVT = getPointerTy(DAG.getDataLayout());

  TLSModel::Model model = getTargetMachine().getTLSModel(GV);

  if (model == TLSModel::GeneralDynamic || model == TLSModel::LocalDynamic) {
    unsigned HiTF = ((model == TLSModel::GeneralDynamic)
                     ? SimMCExpr::VK_Sim_TLS_GD_HI22
                     : SimMCExpr::VK_Sim_TLS_LDM_HI22);
    unsigned LoTF = ((model == TLSModel::GeneralDynamic)
                     ? SimMCExpr::VK_Sim_TLS_GD_LO10
                     : SimMCExpr::VK_Sim_TLS_LDM_LO10);

    SDValue HiLo = makeHiLoPair(Op, HiTF, LoTF, DAG);
    SDValue Base = DAG.getNode(SIMISD::GLOBAL_BASE_REG, DL, PtrVT);
    SDValue Argument = DAG.getNode(SIMISD::TLS_ADD, DL, PtrVT, Base, HiLo,
                               withTargetFlags(Op, 0, DAG));

    SDValue Chain = DAG.getEntryNode();
    SDValue InFlag;

    Chain = DAG.getCALLSEQ_START(Chain, 1, 0, DL);
    Chain = DAG.getCopyToReg(Chain, DL, SIM::O0, Argument, InFlag);
    InFlag = Chain.getValue(1);
    SDValue Callee = DAG.getTargetExternalSymbol("__tls_get_addr", PtrVT);
    SDValue Symbol = withTargetFlags(Op, 0, DAG);

    SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
    const uint32_t *Mask = Subtarget->getRegisterInfo()->getCallPreservedMask(
        DAG.getMachineFunction(), CallingConv::C);
    assert(Mask && "Missing call preserved mask for calling convention");
    SDValue Ops[] = {Chain,
                     Callee,
                     Symbol,
                     DAG.getRegister(SIM::O0, PtrVT),
                     DAG.getRegisterMask(Mask),
                     InFlag};
    Chain = DAG.getNode(SIMISD::TLS_CALL, DL, NodeTys, Ops);
    InFlag = Chain.getValue(1);
    Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(1, DL, true),
                               DAG.getIntPtrConstant(0, DL, true), InFlag, DL);
    InFlag = Chain.getValue(1);
    SDValue Ret = DAG.getCopyFromReg(Chain, DL, SIM::O0, PtrVT, InFlag);

    if (model != TLSModel::LocalDynamic)
      return Ret;

    SDValue Hi = DAG.getNode(SIMISD::Hi, DL, PtrVT,
                 withTargetFlags(Op, 0, DAG));
    SDValue Lo = DAG.getNode(SIMISD::Lo, DL, PtrVT,
                 withTargetFlags(Op, 0, DAG));
    HiLo =  DAG.getNode(ISD::XOR, DL, PtrVT, Hi, Lo);
    return DAG.getNode(SIMISD::TLS_ADD, DL, PtrVT, Ret, HiLo,
                   withTargetFlags(Op, 0, DAG));
  }

  if (model == TLSModel::InitialExec) {

    SDValue Base = DAG.getNode(SIMISD::GLOBAL_BASE_REG, DL, PtrVT);

    // GLOBAL_BASE_REG codegen'ed with call. Inform MFI that this
    // function has calls.
    MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
    MFI.setHasCalls(true);

    SDValue TGA = makeHiLoPair(Op,
                               SimMCExpr::VK_Sim_TLS_IE_HI22,
                               SimMCExpr::VK_Sim_TLS_IE_LO10, DAG);
    SDValue Ptr = DAG.getNode(ISD::ADD, DL, PtrVT, Base, TGA);
    SDValue Offset = DAG.getNode(SIMISD::TLS_LD,
                                 DL, PtrVT, Ptr,
                                 withTargetFlags(Op, 0, DAG));
    return DAG.getNode(SIMISD::TLS_ADD, DL, PtrVT,
                       DAG.getRegister(SIM::G7, PtrVT), Offset,
                       withTargetFlags(Op, 0, DAG));
  }

  assert(model == TLSModel::LocalExec);
  SDValue Hi = DAG.getNode(SIMISD::Hi, DL, PtrVT,
                  withTargetFlags(Op, 0, DAG));
  SDValue Lo = DAG.getNode(SIMISD::Lo, DL, PtrVT,
                  withTargetFlags(Op, 0, DAG));
  SDValue Offset =  DAG.getNode(ISD::XOR, DL, PtrVT, Hi, Lo);

  return DAG.getNode(ISD::ADD, DL, PtrVT,
                     DAG.getRegister(SIM::G7, PtrVT), Offset);
}

SDValue
SimTargetLowering::LowerF128Op(SDValue Op, SelectionDAG &DAG,
                                 const char *LibFuncName,
                                 unsigned numArgs) const {

  ArgListTy Args;

  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
  auto PtrVT = getPointerTy(DAG.getDataLayout());

  SDValue Callee = DAG.getExternalSymbol(LibFuncName, PtrVT);
  Type *RetTy = Op.getValueType().getTypeForEVT(*DAG.getContext());
  Type *RetTyABI = RetTy;
  SDValue Chain = DAG.getEntryNode();
  SDValue RetPtr;

  if (RetTy->isFP128Ty()) {
    // Create a Stack Object to receive the return value of type f128.
    ArgListEntry Entry;
    int RetFI = MFI.CreateStackObject(16, Align(8), false);
    RetPtr = DAG.getFrameIndex(RetFI, PtrVT);
    Entry.Node = RetPtr;
    Entry.Ty   = PointerType::getUnqual(RetTy);
    Entry.IsSRet = true;
    Entry.IsReturned = false;
    Args.push_back(Entry);
    RetTyABI = Type::getVoidTy(*DAG.getContext());
  }

  assert(Op->getNumOperands() >= numArgs && "Not enough operands!");
  for (unsigned i = 0, e = numArgs; i != e; ++i) {
    Chain = LowerF128_LibCallArg(Chain, Args, Op.getOperand(i), SDLoc(Op), DAG);
  }
  TargetLowering::CallLoweringInfo CLI(DAG);
  CLI.setDebugLoc(SDLoc(Op)).setChain(Chain)
    .setCallee(CallingConv::C, RetTyABI, Callee, std::move(Args));

  std::pair<SDValue, SDValue> CallInfo = LowerCallTo(CLI);

  // chain is in second result.
  if (RetTyABI == RetTy)
    return CallInfo.first;

  assert (RetTy->isFP128Ty() && "Unexpected return type!");

  Chain = CallInfo.second;

  // Load RetPtr to get the return value.
  return DAG.getLoad(Op.getValueType(), SDLoc(Op), Chain, RetPtr,
                     MachinePointerInfo(), Align(8));
}

static SDValue LowerFP_TO_SINT(SDValue Op, SelectionDAG &DAG,
                               const SimTargetLowering &TLI,
                               bool hasHardQuad) {
  SDLoc dl(Op);
  EVT VT = Op.getValueType();
  assert(VT == MVT::i32 || VT == MVT::i64);
  assert(Op.getOperand(0).getValueType() != MVT::f128);

  // TODO: remove it
  // Expand f128 operations to fp128 abi calls.
  // if (Op.getOperand(0).getValueType() == MVT::f128
  //     && (!hasHardQuad || !TLI.isTypeLegal(VT))) {
  //   const char *libName = TLI.getLibcallName(VT == MVT::i32
  //                                            ? RTLIB::FPTOSINT_F128_I32
  //                                            : RTLIB::FPTOSINT_F128_I64);
  //   return TLI.LowerF128Op(Op, DAG, libName, 1);
  // }

  // Expand if the resulting type is illegal.
  if (!TLI.isTypeLegal(VT))
    return SDValue();

  // Otherwise, Convert the fp value to integer in an FP register.
  if (VT == MVT::i32)
    Op = DAG.getNode(SIMISD::FTOI, dl, MVT::f32, Op.getOperand(0));
  else
    Op = DAG.getNode(SIMISD::FTOX, dl, MVT::f64, Op.getOperand(0));

  return DAG.getNode(ISD::BITCAST, dl, VT, Op);
}

static SDValue LowerSINT_TO_FP(SDValue Op, SelectionDAG &DAG,
                               const SimTargetLowering &TLI,
                               bool hasHardQuad) {
  SDLoc dl(Op);
  EVT OpVT = Op.getOperand(0).getValueType();
  assert(OpVT == MVT::i32 || (OpVT == MVT::i64));

  EVT floatVT = (OpVT == MVT::i32) ? MVT::f32 : MVT::f64;

  assert(Op.getValueType() != MVT::f128);
  // TODO: remove it
  // Expand f128 operations to fp128 ABI calls.
  // if (Op.getValueType() == MVT::f128
  //     && (!hasHardQuad || !TLI.isTypeLegal(OpVT))) {
  //   const char *libName = TLI.getLibcallName(OpVT == MVT::i32
  //                                            ? RTLIB::SINTTOFP_I32_F128
  //                                            : RTLIB::SINTTOFP_I64_F128);
  //   return TLI.LowerF128Op(Op, DAG, libName, 1);
  // }

  // Expand if the operand type is illegal.
  if (!TLI.isTypeLegal(OpVT))
    return SDValue();

  // Otherwise, Convert the int value to FP in an FP register.
  SDValue Tmp = DAG.getNode(ISD::BITCAST, dl, floatVT, Op.getOperand(0));
  unsigned opcode = (OpVT == MVT::i32)? SIMISD::ITOF : SIMISD::XTOF;
  return DAG.getNode(opcode, dl, Op.getValueType(), Tmp);
}

static SDValue LowerFP_TO_UINT(SDValue Op, SelectionDAG &DAG,
                               const SimTargetLowering &TLI,
                               bool hasHardQuad) {
  SDLoc dl(Op);
  EVT VT = Op.getValueType();

  // Expand if it does not involve f128 or the target has support for
  // quad floating point instructions and the resulting type is legal.
  if (Op.getOperand(0).getValueType() != MVT::f128 ||
      (hasHardQuad && TLI.isTypeLegal(VT)))
    return SDValue();

  assert(VT == MVT::i32 || VT == MVT::i64);

  return TLI.LowerF128Op(Op, DAG,
                         TLI.getLibcallName(VT == MVT::i32
                                            ? RTLIB::FPTOUINT_F128_I32
                                            : RTLIB::FPTOUINT_F128_I64),
                         1);
}

static SDValue LowerUINT_TO_FP(SDValue Op, SelectionDAG &DAG,
                               const SimTargetLowering &TLI,
                               bool hasHardQuad) {
  SDLoc dl(Op);
  EVT OpVT = Op.getOperand(0).getValueType();
  assert(OpVT == MVT::i32 || OpVT == MVT::i64);

  // Expand if it does not involve f128 or the target has support for
  // quad floating point instructions and the operand type is legal.
  if (Op.getValueType() != MVT::f128 || (hasHardQuad && TLI.isTypeLegal(OpVT)))
    return SDValue();

  return TLI.LowerF128Op(Op, DAG,
                         TLI.getLibcallName(OpVT == MVT::i32
                                            ? RTLIB::UINTTOFP_I32_F128
                                            : RTLIB::UINTTOFP_I64_F128),
                         1);
}

// TODO: remove
// static SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG,
//                           const SimTargetLowering &TLI,
//                           bool hasHardQuad) {
//   SDValue Chain = Op.getOperand(0);
//   ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
//   SDValue LHS = Op.getOperand(2);
//   SDValue RHS = Op.getOperand(3);
//   SDValue Dest = Op.getOperand(4);
//   SDLoc dl(Op);
//   unsigned Opc, SPCC = ~0U;

//   // If this is a br_cc of a "setcc", and if the setcc got lowered into
//   // an CMP[IF]CC/SELECT_[IF]CC pair, find the original compared values.
//   LookThroughSetCC(LHS, RHS, CC, SPCC);

//   // Get the condition flag.
//   SDValue CompareFlag;
//   if (LHS.getValueType().isInteger()) {
//     CompareFlag = DAG.getNode(SIMISD::CMPICC, dl, MVT::Glue, LHS, RHS);
//     // 32-bit compares use the icc flags, 64-bit uses the xcc flags.
//     Opc = LHS.getValueType() == MVT::i32 ? SIMISD::BRICC : SIMISD::BRXCC;
//   } else {
//     assert(LHS.getValueType() != MVT::f128);
//     CompareFlag = DAG.getNode(SIMISD::CMPFCC, dl, MVT::Glue, LHS, RHS);
//     Opc = SIMISD::BRFCC;
//   }
//   return DAG.getNode(Opc, dl, MVT::Other, Chain, Dest,
//                      DAG.getConstant(SPCC, dl, MVT::i32), CompareFlag);
// }

// static SDValue LowerSELECT_CC(SDValue Op, SelectionDAG &DAG,
//                               const SimTargetLowering &TLI,
//                               bool hasHardQuad) {
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
//     CompareFlag = DAG.getNode(SIMISD::CMPICC, dl, MVT::Glue, LHS, RHS);
//     Opc = LHS.getValueType() == MVT::i32 ?
//           SIMISD::SELECT_ICC : SIMISD::SELECT_XCC;
//   } else {
//     CompareFlag = DAG.getNode(SIMISD::CMPFCC, dl, MVT::Glue, LHS, RHS);
//     Opc = SIMISD::SELECT_FCC;
//   }
//   return DAG.getNode(Opc, dl, TrueVal.getValueType(), TrueVal, FalseVal,
//                      DAG.getConstant(SPCC, dl, MVT::i32), CompareFlag);
// }

static SDValue LowerVASTART(SDValue Op, SelectionDAG &DAG,
                            const SimTargetLowering &TLI) {
  MachineFunction &MF = DAG.getMachineFunction();
  SimMachineFunctionInfo *FuncInfo = MF.getInfo<SimMachineFunctionInfo>();
  auto PtrVT = TLI.getPointerTy(DAG.getDataLayout());

  // Need frame address to find the address of VarArgsFrameIndex.
  MF.getFrameInfo().setFrameAddressIsTaken(true);

  // vastart just stores the address of the VarArgsFrameIndex slot into the
  // memory location argument.
  SDLoc DL(Op);
  SDValue Offset =
      DAG.getNode(ISD::ADD, DL, PtrVT, DAG.getRegister(SIM::R14, PtrVT),
                  DAG.getIntPtrConstant(FuncInfo->getVarArgsFrameOffset(), DL));
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  return DAG.getStore(Op.getOperand(0), DL, Offset, Op.getOperand(1),
                      MachinePointerInfo(SV));
}

static SDValue LowerVAARG(SDValue Op, SelectionDAG &DAG) {
  SDNode *Node = Op.getNode();
  EVT VT = Node->getValueType(0);
  SDValue InChain = Node->getOperand(0);
  SDValue VAListPtr = Node->getOperand(1);
  EVT PtrVT = VAListPtr.getValueType();
  const Value *SV = cast<SrcValueSDNode>(Node->getOperand(2))->getValue();
  SDLoc DL(Node);
  SDValue VAList =
      DAG.getLoad(PtrVT, DL, InChain, VAListPtr, MachinePointerInfo(SV));
  // Increment the pointer, VAList, to the next vaarg.
  SDValue NextPtr = DAG.getNode(ISD::ADD, DL, PtrVT, VAList,
                                DAG.getIntPtrConstant(VT.getSizeInBits()/8,
                                                      DL));
  // Store the incremented VAList to the legalized pointer.
  InChain = DAG.getStore(VAList.getValue(1), DL, NextPtr, VAListPtr,
                         MachinePointerInfo(SV));
  // Load the actual argument out of the pointer VAList.
  // We can't count on greater alignment than the word size.
  return DAG.getLoad(
      VT, DL, InChain, VAList, MachinePointerInfo(),
      std::min(PtrVT.getFixedSizeInBits(), VT.getFixedSizeInBits()) / 8);
}

static SDValue LowerDYNAMIC_STACKALLOC(SDValue Op, SelectionDAG &DAG,
                                       const SimSubtarget *Subtarget) {
  SDValue Chain = Op.getOperand(0);  // Legalize the chain.
  SDValue Size  = Op.getOperand(1);  // Legalize the size.
  MaybeAlign Alignment =
      cast<ConstantSDNode>(Op.getOperand(2))->getMaybeAlignValue();
  Align StackAlign = Subtarget->getFrameLowering()->getStackAlign();
  EVT VT = Size->getValueType(0);
  SDLoc dl(Op);

  // TODO: implement over-aligned alloca. (Note: also implies
  // supporting support for overaligned function frames + dynamic
  // allocations, at all, which currently isn't supported)
  if (Alignment && *Alignment > StackAlign) {
    const MachineFunction &MF = DAG.getMachineFunction();
    report_fatal_error("Function \"" + Twine(MF.getName()) + "\": "
                       "over-aligned dynamic alloca not supported.");
  }

  // On Sim32, the size of the spill area is 92. Unfortunately,
  // that's only 4-byte aligned, not 8-byte aligned (the stack
  // pointer is 8-byte aligned). So, if the user asked for an 8-byte
  // aligned dynamic allocation, we actually need to add 96 to the
  // bottom of the stack, instead of 92, to ensure 8-byte alignment.

  // That also means adding 4 to the size of the allocation --
  // before applying the 8-byte rounding. Unfortunately, we the
  // value we get here has already had rounding applied. So, we need
  // to add 8, instead, wasting a bit more memory.

  // Further, this only actually needs to be done if the required
  // alignment is > 4, but, we've lost that info by this point, too,
  // so we always apply it.

  // (An alternative approach would be to always reserve 96 bytes
  // instead of the required 92, but then we'd waste 4 extra bytes
  // in every frame, not just those with dynamic stack allocations)

  // TODO: modify code in SelectionDAGBuilder to make this less sad.

  Size = DAG.getNode(ISD::ADD, dl, VT, Size,
                     DAG.getConstant(8, dl, VT));
  unsigned regSpillArea = 96;

  unsigned SPReg = SIM::R15;
  SDValue SP = DAG.getCopyFromReg(Chain, dl, SPReg, VT);
  SDValue NewSP = DAG.getNode(ISD::SUB, dl, VT, SP, Size); // Value
  Chain = DAG.getCopyToReg(SP.getValue(1), dl, SPReg, NewSP);    // Output chain

  regSpillArea += Subtarget->getStackPointerBias();

  SDValue NewVal = DAG.getNode(ISD::ADD, dl, VT, NewSP,
                               DAG.getConstant(regSpillArea, dl, VT));
  SDValue Ops[2] = { NewVal, Chain };
  return DAG.getMergeValues(Ops, dl);
}


static SDValue getFLUSHW(SDValue Op, SelectionDAG &DAG) {
  SDLoc dl(Op);
  SDValue Chain = DAG.getNode(SIMISD::FLUSHW,
                              dl, MVT::Other, DAG.getEntryNode());
  return Chain;
}

static SDValue getFRAMEADDR(uint64_t depth, SDValue Op, SelectionDAG &DAG,
                            const SimSubtarget *Subtarget,
                            bool AlwaysFlush = false) {
  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
  MFI.setFrameAddressIsTaken(true);

  EVT VT = Op.getValueType();
  SDLoc dl(Op);
  // TODO: change FrameReg
  unsigned FrameReg = SIM::R14;
  unsigned stackBias = Subtarget->getStackPointerBias();

  SDValue FrameAddr;
  SDValue Chain;

  // flush first to make sure the windowed registers' values are in stack
  Chain = (depth || AlwaysFlush) ? getFLUSHW(Op, DAG) : DAG.getEntryNode();

  FrameAddr = DAG.getCopyFromReg(Chain, dl, FrameReg, VT);

  unsigned Offset = 56;

  while (depth--) {
    SDValue Ptr = DAG.getNode(ISD::ADD, dl, VT, FrameAddr,
                              DAG.getIntPtrConstant(Offset, dl));
    FrameAddr = DAG.getLoad(VT, dl, Chain, Ptr, MachinePointerInfo());
  }
  return FrameAddr;
}


static SDValue LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG,
                              const SimSubtarget *Subtarget) {

  uint64_t depth = Op.getConstantOperandVal(0);

  return getFRAMEADDR(depth, Op, DAG, Subtarget);

}

static SDValue LowerRETURNADDR(SDValue Op, SelectionDAG &DAG,
                               const SimTargetLowering &TLI,
                               const SimSubtarget *Subtarget) {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MFI.setReturnAddressIsTaken(true);

  if (TLI.verifyReturnAddressArgumentIsConstant(Op, DAG))
    return SDValue();

  EVT VT = Op.getValueType();
  SDLoc dl(Op);
  uint64_t depth = Op.getConstantOperandVal(0);

  SDValue RetAddr;
  if (depth == 0) {
    auto PtrVT = TLI.getPointerTy(DAG.getDataLayout());
    // TODO: choose reg
    unsigned RetReg = MF.addLiveIn(SIM::R14, TLI.getRegClassFor(PtrVT));
    RetAddr = DAG.getCopyFromReg(DAG.getEntryNode(), dl, RetReg, VT);
    return RetAddr;
  }

  // Need frame address to find return address of the caller.
  SDValue FrameAddr = getFRAMEADDR(depth - 1, Op, DAG, Subtarget, true);

  unsigned Offset = 60;
  SDValue Ptr = DAG.getNode(ISD::ADD,
                            dl, VT,
                            FrameAddr,
                            DAG.getIntPtrConstant(Offset, dl));
  RetAddr = DAG.getLoad(VT, dl, DAG.getEntryNode(), Ptr, MachinePointerInfo());

  return RetAddr;
}

static SDValue LowerF64Op(SDValue SrcReg64, const SDLoc &dl, SelectionDAG &DAG,
                          unsigned opcode) {
  assert(SrcReg64.getValueType() == MVT::f64 && "LowerF64Op called on non-double!");
  assert(opcode == ISD::FNEG || opcode == ISD::FABS);

  // Lower fneg/fabs on f64 to fneg/fabs on f32.
  // fneg f64 => fneg f32:sub_even, fmov f32:sub_odd.
  // fabs f64 => fabs f32:sub_even, fmov f32:sub_odd.

  // Note: in little-endian, the floating-point value is stored in the
  // registers are in the opposite order, so the subreg with the sign
  // bit is the highest-numbered (odd), rather than the
  // lowest-numbered (even).

  SDValue Hi32 = DAG.getTargetExtractSubreg(SIM::sub_even, dl, MVT::f32,
                                            SrcReg64);
  SDValue Lo32 = DAG.getTargetExtractSubreg(SIM::sub_odd, dl, MVT::f32,
                                            SrcReg64);

  if (DAG.getDataLayout().isLittleEndian())
    Lo32 = DAG.getNode(opcode, dl, MVT::f32, Lo32);
  else
    Hi32 = DAG.getNode(opcode, dl, MVT::f32, Hi32);

  SDValue DstReg64 = SDValue(DAG.getMachineNode(TargetOpcode::IMPLICIT_DEF,
                                                dl, MVT::f64), 0);
  DstReg64 = DAG.getTargetInsertSubreg(SIM::sub_even, dl, MVT::f64,
                                       DstReg64, Hi32);
  DstReg64 = DAG.getTargetInsertSubreg(SIM::sub_odd, dl, MVT::f64,
                                       DstReg64, Lo32);
  return DstReg64;
}

// Lower a f128 load into two f64 loads.
static SDValue LowerF128Load(SDValue Op, SelectionDAG &DAG)
{
  // TODO: remove function
  assert(false);
  SDLoc dl(Op);
  LoadSDNode *LdNode = cast<LoadSDNode>(Op.getNode());
  assert(LdNode->getOffset().isUndef() && "Unexpected node type");

  Align Alignment = commonAlignment(LdNode->getOriginalAlign(), 8);

  SDValue Hi64 =
      DAG.getLoad(MVT::f64, dl, LdNode->getChain(), LdNode->getBasePtr(),
                  LdNode->getPointerInfo(), Alignment);
  EVT addrVT = LdNode->getBasePtr().getValueType();
  SDValue LoPtr = DAG.getNode(ISD::ADD, dl, addrVT,
                              LdNode->getBasePtr(),
                              DAG.getConstant(8, dl, addrVT));
  SDValue Lo64 = DAG.getLoad(MVT::f64, dl, LdNode->getChain(), LoPtr,
                             LdNode->getPointerInfo().getWithOffset(8),
                             Alignment);

  SDValue SubRegEven = DAG.getTargetConstant(SIM::sub_even64, dl, MVT::i32);
  SDValue SubRegOdd  = DAG.getTargetConstant(SIM::sub_odd64, dl, MVT::i32);

  SDNode *InFP128 = DAG.getMachineNode(TargetOpcode::IMPLICIT_DEF,
                                       dl, MVT::f128);
  InFP128 = DAG.getMachineNode(TargetOpcode::INSERT_SUBREG, dl,
                               MVT::f128,
                               SDValue(InFP128, 0),
                               Hi64,
                               SubRegEven);
  InFP128 = DAG.getMachineNode(TargetOpcode::INSERT_SUBREG, dl,
                               MVT::f128,
                               SDValue(InFP128, 0),
                               Lo64,
                               SubRegOdd);
  SDValue OutChains[2] = { SDValue(Hi64.getNode(), 1),
                           SDValue(Lo64.getNode(), 1) };
  SDValue OutChain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains);
  SDValue Ops[2] = {SDValue(InFP128,0), OutChain};
  return DAG.getMergeValues(Ops, dl);
}

static SDValue LowerLOAD(SDValue Op, SelectionDAG &DAG)
{
  LoadSDNode *LdNode = cast<LoadSDNode>(Op.getNode());

  EVT MemVT = LdNode->getMemoryVT();
  if (MemVT == MVT::f128)
    return LowerF128Load(Op, DAG);

  return Op;
}

// Lower a f128 store into two f64 stores.
static SDValue LowerF128Store(SDValue Op, SelectionDAG &DAG) {
  // TODO: remove function
  assert(false);
  SDLoc dl(Op);
  StoreSDNode *StNode = cast<StoreSDNode>(Op.getNode());
  assert(StNode->getOffset().isUndef() && "Unexpected node type");

  SDValue SubRegEven = DAG.getTargetConstant(SIM::sub_even64, dl, MVT::i32);
  SDValue SubRegOdd  = DAG.getTargetConstant(SIM::sub_odd64, dl, MVT::i32);

  SDNode *Hi64 = DAG.getMachineNode(TargetOpcode::EXTRACT_SUBREG,
                                    dl,
                                    MVT::f64,
                                    StNode->getValue(),
                                    SubRegEven);
  SDNode *Lo64 = DAG.getMachineNode(TargetOpcode::EXTRACT_SUBREG,
                                    dl,
                                    MVT::f64,
                                    StNode->getValue(),
                                    SubRegOdd);

  Align Alignment = commonAlignment(StNode->getOriginalAlign(), 8);

  SDValue OutChains[2];
  OutChains[0] =
      DAG.getStore(StNode->getChain(), dl, SDValue(Hi64, 0),
                   StNode->getBasePtr(), StNode->getPointerInfo(),
                   Alignment);
  EVT addrVT = StNode->getBasePtr().getValueType();
  SDValue LoPtr = DAG.getNode(ISD::ADD, dl, addrVT,
                              StNode->getBasePtr(),
                              DAG.getConstant(8, dl, addrVT));
  OutChains[1] = DAG.getStore(StNode->getChain(), dl, SDValue(Lo64, 0), LoPtr,
                              StNode->getPointerInfo().getWithOffset(8),
                              Alignment);
  return DAG.getNode(ISD::TokenFactor, dl, MVT::Other, OutChains);
}

static SDValue LowerSTORE(SDValue Op, SelectionDAG &DAG)
{
  SDLoc dl(Op);
  StoreSDNode *St = cast<StoreSDNode>(Op.getNode());

  EVT MemVT = St->getMemoryVT();
  if (MemVT == MVT::f128)
    return LowerF128Store(Op, DAG);

  if (MemVT == MVT::i64) {
    // Custom handling for i64 stores: turn it into a bitcast and a
    // v2i32 store.
    SDValue Val = DAG.getNode(ISD::BITCAST, dl, MVT::v2i32, St->getValue());
    SDValue Chain = DAG.getStore(
        St->getChain(), dl, Val, St->getBasePtr(), St->getPointerInfo(),
        St->getOriginalAlign(), St->getMemOperand()->getFlags(),
        St->getAAInfo());
    return Chain;
  }

  return SDValue();
}

static SDValue LowerFNEGorFABS(SDValue Op, SelectionDAG &DAG, bool isV9) {
  assert((Op.getOpcode() == ISD::FNEG || Op.getOpcode() == ISD::FABS)
         && "invalid opcode");

  SDLoc dl(Op);

  if (Op.getValueType() == MVT::f64)
    return LowerF64Op(Op.getOperand(0), dl, DAG, Op.getOpcode());
  if (Op.getValueType() != MVT::f128)
    return Op;

  // Lower fabs/fneg on f128 to fabs/fneg on f64
  // fabs/fneg f128 => fabs/fneg f64:sub_even64, fmov f64:sub_odd64
  // (As with LowerF64Op, on little-endian, we need to negate the odd
  // subreg)

  SDValue SrcReg128 = Op.getOperand(0);
  SDValue Hi64 = DAG.getTargetExtractSubreg(SIM::sub_even64, dl, MVT::f64,
                                            SrcReg128);
  SDValue Lo64 = DAG.getTargetExtractSubreg(SIM::sub_odd64, dl, MVT::f64,
                                            SrcReg128);

  if (DAG.getDataLayout().isLittleEndian()) {
    if (isV9)
      Lo64 = DAG.getNode(Op.getOpcode(), dl, MVT::f64, Lo64);
    else
      Lo64 = LowerF64Op(Lo64, dl, DAG, Op.getOpcode());
  } else {
    if (isV9)
      Hi64 = DAG.getNode(Op.getOpcode(), dl, MVT::f64, Hi64);
    else
      Hi64 = LowerF64Op(Hi64, dl, DAG, Op.getOpcode());
  }

  SDValue DstReg128 = SDValue(DAG.getMachineNode(TargetOpcode::IMPLICIT_DEF,
                                                 dl, MVT::f128), 0);
  DstReg128 = DAG.getTargetInsertSubreg(SIM::sub_even64, dl, MVT::f128,
                                        DstReg128, Hi64);
  DstReg128 = DAG.getTargetInsertSubreg(SIM::sub_odd64, dl, MVT::f128,
                                        DstReg128, Lo64);
  return DstReg128;
}

static SDValue LowerADDC_ADDE_SUBC_SUBE(SDValue Op, SelectionDAG &DAG) {

  if (Op.getValueType() != MVT::i64)
    return Op;

  SDLoc dl(Op);
  SDValue Src1 = Op.getOperand(0);
  SDValue Src1Lo = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, Src1);
  SDValue Src1Hi = DAG.getNode(ISD::SRL, dl, MVT::i64, Src1,
                               DAG.getConstant(32, dl, MVT::i64));
  Src1Hi = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, Src1Hi);

  SDValue Src2 = Op.getOperand(1);
  SDValue Src2Lo = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, Src2);
  SDValue Src2Hi = DAG.getNode(ISD::SRL, dl, MVT::i64, Src2,
                               DAG.getConstant(32, dl, MVT::i64));
  Src2Hi = DAG.getNode(ISD::TRUNCATE, dl, MVT::i32, Src2Hi);


  bool hasChain = false;
  unsigned hiOpc = Op.getOpcode();
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Invalid opcode");
  case ISD::ADDC: hiOpc = ISD::ADDE; break;
  case ISD::ADDE: hasChain = true; break;
  case ISD::SUBC: hiOpc = ISD::SUBE; break;
  case ISD::SUBE: hasChain = true; break;
  }
  SDValue Lo;
  SDVTList VTs = DAG.getVTList(MVT::i32, MVT::Glue);
  if (hasChain) {
    Lo = DAG.getNode(Op.getOpcode(), dl, VTs, Src1Lo, Src2Lo,
                     Op.getOperand(2));
  } else {
    Lo = DAG.getNode(Op.getOpcode(), dl, VTs, Src1Lo, Src2Lo);
  }
  SDValue Hi = DAG.getNode(hiOpc, dl, VTs, Src1Hi, Src2Hi, Lo.getValue(1));
  SDValue Carry = Hi.getValue(1);

  Lo = DAG.getNode(ISD::ZERO_EXTEND, dl, MVT::i64, Lo);
  Hi = DAG.getNode(ISD::ZERO_EXTEND, dl, MVT::i64, Hi);
  Hi = DAG.getNode(ISD::SHL, dl, MVT::i64, Hi,
                   DAG.getConstant(32, dl, MVT::i64));

  SDValue Dst = DAG.getNode(ISD::OR, dl, MVT::i64, Hi, Lo);
  SDValue Ops[2] = { Dst, Carry };
  return DAG.getMergeValues(Ops, dl);
}

// Custom lower UMULO/SMULO for Sim. This code is similar to ExpandNode()
// in LegalizeDAG.cpp except the order of arguments to the library function.
static SDValue LowerUMULO_SMULO(SDValue Op, SelectionDAG &DAG,
                                const SimTargetLowering &TLI)
{
  unsigned opcode = Op.getOpcode();
  assert((opcode == ISD::UMULO || opcode == ISD::SMULO) && "Invalid Opcode.");

  bool isSigned = (opcode == ISD::SMULO);
  EVT VT = MVT::i64;
  EVT WideVT = MVT::i128;
  SDLoc dl(Op);
  SDValue LHS = Op.getOperand(0);

  if (LHS.getValueType() != VT)
    return Op;

  SDValue ShiftAmt = DAG.getConstant(63, dl, VT);

  SDValue RHS = Op.getOperand(1);
  SDValue HiLHS, HiRHS;
  if (isSigned) {
    HiLHS = DAG.getNode(ISD::SRA, dl, VT, LHS, ShiftAmt);
    HiRHS = DAG.getNode(ISD::SRA, dl, MVT::i64, RHS, ShiftAmt);
  } else {
    HiLHS = DAG.getConstant(0, dl, VT);
    HiRHS = DAG.getConstant(0, dl, MVT::i64);
  }

  SDValue Args[] = { HiLHS, LHS, HiRHS, RHS };

  TargetLowering::MakeLibCallOptions CallOptions;
  CallOptions.setSExt(isSigned);
  SDValue MulResult = TLI.makeLibCall(DAG,
                                      RTLIB::MUL_I128, WideVT,
                                      Args, CallOptions, dl).first;
  SDValue BottomHalf = DAG.getNode(ISD::EXTRACT_ELEMENT, dl, VT,
                                   MulResult, DAG.getIntPtrConstant(0, dl));
  SDValue TopHalf = DAG.getNode(ISD::EXTRACT_ELEMENT, dl, VT,
                                MulResult, DAG.getIntPtrConstant(1, dl));
  if (isSigned) {
    SDValue Tmp1 = DAG.getNode(ISD::SRA, dl, VT, BottomHalf, ShiftAmt);
    TopHalf = DAG.getSetCC(dl, MVT::i32, TopHalf, Tmp1, ISD::SETNE);
  } else {
    TopHalf = DAG.getSetCC(dl, MVT::i32, TopHalf, DAG.getConstant(0, dl, VT),
                           ISD::SETNE);
  }
  // MulResult is a node with an illegal type. Because such things are not
  // generally permitted during this phase of legalization, ensure that
  // nothing is left using the node. The above EXTRACT_ELEMENT nodes should have
  // been folded.
  assert(MulResult->use_empty() && "Illegally typed node still in use!");

  SDValue Ops[2] = { BottomHalf, TopHalf } ;
  return DAG.getMergeValues(Ops, dl);
}

SDValue SimTargetLowering::LowerINTRINSIC_WO_CHAIN(SDValue Op,
                                                     SelectionDAG &DAG) const {
  unsigned IntNo = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();
  SDLoc dl(Op);
  switch (IntNo) {
  default: return SDValue();    // Don't custom lower most intrinsics.
  case Intrinsic::thread_pointer: {
    EVT PtrVT = getPointerTy(DAG.getDataLayout());
    return DAG.getRegister(SIM::G7, PtrVT);
  }
  }
}

SDValue SimTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {

  bool hasHardQuad = Subtarget->hasHardQuad();

  switch (Op.getOpcode()) {
  default: llvm_unreachable("Should not custom lower this!");

  case ISD::RETURNADDR:         return LowerRETURNADDR(Op, DAG, *this,
                                                       Subtarget);
  case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG,
                                                      Subtarget);
  case ISD::GlobalTLSAddress:   return LowerGlobalTLSAddress(Op, DAG);
  case ISD::GlobalAddress:      return LowerGlobalAddress(Op, DAG);
  case ISD::BlockAddress:       return LowerBlockAddress(Op, DAG);
  case ISD::ConstantPool:       return LowerConstantPool(Op, DAG);
  case ISD::FP_TO_SINT:         return LowerFP_TO_SINT(Op, DAG, *this,
                                                       hasHardQuad);
  case ISD::SINT_TO_FP:         return LowerSINT_TO_FP(Op, DAG, *this,
                                                       hasHardQuad);
  case ISD::FP_TO_UINT:         return LowerFP_TO_UINT(Op, DAG, *this,
                                                       hasHardQuad);
  case ISD::UINT_TO_FP:         return LowerUINT_TO_FP(Op, DAG, *this,
                                                       hasHardQuad);
  // case ISD::BR_CC:              return LowerBR_CC(Op, DAG, *this,
  //                                                 hasHardQuad);
  // case ISD::SELECT_CC:          return LowerSELECT_CC(Op, DAG, *this,
  //                                                     hasHardQuad);
  case ISD::VASTART:            return LowerVASTART(Op, DAG, *this);
  case ISD::VAARG:              return LowerVAARG(Op, DAG);
  case ISD::DYNAMIC_STACKALLOC: return LowerDYNAMIC_STACKALLOC(Op, DAG,
                                                               Subtarget);

  case ISD::LOAD:               return LowerLOAD(Op, DAG);
  case ISD::STORE:              return LowerSTORE(Op, DAG);
  case ISD::FADD:               return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::ADD_F128), 2);
  case ISD::FSUB:               return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::SUB_F128), 2);
  case ISD::FMUL:               return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::MUL_F128), 2);
  case ISD::FDIV:               return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::DIV_F128), 2);
  case ISD::FSQRT:              return LowerF128Op(Op, DAG,
                                       getLibcallName(RTLIB::SQRT_F128),1);
  case ISD::FABS:
  case ISD::FNEG:               return LowerFNEGorFABS(Op, DAG, false);
  case ISD::ADDC:
  case ISD::ADDE:
  case ISD::SUBC:
  case ISD::SUBE:               return LowerADDC_ADDE_SUBC_SUBE(Op, DAG);
  case ISD::UMULO:
  case ISD::SMULO:              return LowerUMULO_SMULO(Op, DAG, *this);
  case ISD::INTRINSIC_WO_CHAIN: return LowerINTRINSIC_WO_CHAIN(Op, DAG);
  }
}

SDValue SimTargetLowering::bitcastConstantFPToInt(ConstantFPSDNode *C,
                                                    const SDLoc &DL,
                                                    SelectionDAG &DAG) const {
  APInt V = C->getValueAPF().bitcastToAPInt();
  SDValue Lo = DAG.getConstant(V.zextOrTrunc(32), DL, MVT::i32);
  SDValue Hi = DAG.getConstant(V.lshr(32).zextOrTrunc(32), DL, MVT::i32);
  if (DAG.getDataLayout().isLittleEndian())
    std::swap(Lo, Hi);
  return DAG.getBuildVector(MVT::v2i32, DL, {Hi, Lo});
}

SDValue SimTargetLowering::PerformBITCASTCombine(SDNode *N,
                                                   DAGCombinerInfo &DCI) const {
  SDLoc dl(N);
  SDValue Src = N->getOperand(0);

  if (isa<ConstantFPSDNode>(Src) && N->getSimpleValueType(0) == MVT::v2i32 &&
      Src.getSimpleValueType() == MVT::f64)
    return bitcastConstantFPToInt(cast<ConstantFPSDNode>(Src), dl, DCI.DAG);

  return SDValue();
}

SDValue SimTargetLowering::PerformDAGCombine(SDNode *N,
                                               DAGCombinerInfo &DCI) const {
  switch (N->getOpcode()) {
  default:
    break;
  case ISD::BITCAST:
    return PerformBITCASTCombine(N, DCI);
  }
  return SDValue();
}

MachineBasicBlock *
SimTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                 MachineBasicBlock *BB) const {
  switch (MI.getOpcode()) {
  default: llvm_unreachable("Unknown SELECT_CC!");
  case SIM::SELECT_CC_Int_ICC:
  case SIM::SELECT_CC_FP_ICC:
  case SIM::SELECT_CC_DFP_ICC:
  case SIM::SELECT_CC_QFP_ICC:
  case SIM::SELECT_CC_Int_FCC:
  case SIM::SELECT_CC_FP_FCC:
  case SIM::SELECT_CC_DFP_FCC:
  case SIM::SELECT_CC_QFP_FCC:
    return expandSelectCC(MI, BB, SIM::BEQ);
  }
}

MachineBasicBlock *
SimTargetLowering::expandSelectCC(MachineInstr &MI, MachineBasicBlock *BB,
                                    unsigned BROpcode) const {
  const TargetInstrInfo &TII = *Subtarget->getInstrInfo();
  DebugLoc dl = MI.getDebugLoc();
  unsigned CC = (SPCC::CondCodes)MI.getOperand(3).getImm();

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
    .addMBB(SinkMBB)
    .addImm(CC);

  // IfFalseMBB just falls through to SinkMBB.
  IfFalseMBB->addSuccessor(SinkMBB);

  // %Result = phi [ %TrueValue, ThisMBB ], [ %FalseValue, IfFalseMBB ]
  BuildMI(*SinkMBB, SinkMBB->begin(), dl, TII.get(SIM::PHI),
          MI.getOperand(0).getReg())
      .addReg(MI.getOperand(1).getReg())
      .addMBB(ThisMBB)
      .addReg(MI.getOperand(2).getReg())
      .addMBB(IfFalseMBB);

  MI.eraseFromParent(); // The pseudo instruction is gone now.
  return SinkMBB;
}

//===----------------------------------------------------------------------===//
//                         Sim Inline Assembly Support
//===----------------------------------------------------------------------===//

/// getConstraintType - Given a constraint letter, return the type of
/// constraint it is for this target.
SimTargetLowering::ConstraintType
SimTargetLowering::getConstraintType(StringRef Constraint) const {
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    default:  break;
    case 'r':
    case 'f':
    case 'e':
      return C_RegisterClass;
    case 'I': // SIMM13
      return C_Immediate;
    }
  }

  return TargetLowering::getConstraintType(Constraint);
}

TargetLowering::ConstraintWeight SimTargetLowering::
getSingleConstraintMatchWeight(AsmOperandInfo &info,
                               const char *constraint) const {
  ConstraintWeight weight = CW_Invalid;
  Value *CallOperandVal = info.CallOperandVal;
  // If we don't have a value, we can't do a match,
  // but allow it at the lowest weight.
  if (!CallOperandVal)
    return CW_Default;

  // Look at the constraint type.
  switch (*constraint) {
  default:
    weight = TargetLowering::getSingleConstraintMatchWeight(info, constraint);
    break;
  case 'I': // SIMM13
    if (ConstantInt *C = dyn_cast<ConstantInt>(info.CallOperandVal)) {
      if (isInt<13>(C->getSExtValue()))
        weight = CW_Constant;
    }
    break;
  }
  return weight;
}

/// LowerAsmOperandForConstraint - Lower the specified operand into the Ops
/// vector.  If it is invalid, don't add anything to Ops.
void SimTargetLowering::
LowerAsmOperandForConstraint(SDValue Op,
                             std::string &Constraint,
                             std::vector<SDValue> &Ops,
                             SelectionDAG &DAG) const {
  SDValue Result(nullptr, 0);

  // Only support length 1 constraints for now.
  if (Constraint.length() > 1)
    return;

  char ConstraintLetter = Constraint[0];
  switch (ConstraintLetter) {
  default: break;
  case 'I':
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
      if (isInt<13>(C->getSExtValue())) {
        Result = DAG.getTargetConstant(C->getSExtValue(), SDLoc(Op),
                                       Op.getValueType());
        break;
      }
      return;
    }
  }

  if (Result.getNode()) {
    Ops.push_back(Result);
    return;
  }
  TargetLowering::LowerAsmOperandForConstraint(Op, Constraint, Ops, DAG);
}

std::pair<unsigned, const TargetRegisterClass *>
SimTargetLowering::getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                                                  StringRef Constraint,
                                                  MVT VT) const {
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'r':
      if (VT == MVT::v2i32)
        return std::make_pair(0U, &SIM::IntPairRegClass);
      else
        return std::make_pair(0U, &SIM::SimGPRReg);
    case 'f':
      if (VT == MVT::f32 || VT == MVT::i32)
        return std::make_pair(0U, &SIM::FPRegsRegClass);
      else if (VT == MVT::f64 || VT == MVT::i64)
        return std::make_pair(0U, &SIM::LowDFPRegsRegClass);
      else if (VT == MVT::f128)
        return std::make_pair(0U, &SIM::LowQFPRegsRegClass);
      // This will generate an error message
      return std::make_pair(0U, nullptr);
    case 'e':
      if (VT == MVT::f32 || VT == MVT::i32)
        return std::make_pair(0U, &SIM::FPRegsRegClass);
      else if (VT == MVT::f64 || VT == MVT::i64 )
        return std::make_pair(0U, &SIM::DFPRegsRegClass);
      else if (VT == MVT::f128)
        return std::make_pair(0U, &SIM::QFPRegsRegClass);
      // This will generate an error message
      return std::make_pair(0U, nullptr);
    }
  } else if (!Constraint.empty() && Constraint.size() <= 5
              && Constraint[0] == '{' && *(Constraint.end()-1) == '}') {
    // constraint = '{r<d>}'
    // Remove the braces from around the name.
    StringRef name(Constraint.data()+1, Constraint.size()-2);
    // Handle register aliases:
    //       r0-r7   -> g0-g7
    //       r8-r15  -> o0-o7
    //       r16-r23 -> l0-l7
    //       r24-r31 -> i0-i7
    uint64_t intVal = 0;
    if (name.substr(0, 1).equals("r")
        && !name.substr(1).getAsInteger(10, intVal) && intVal <= 31) {
      const char regTypes[] = { 'g', 'o', 'l', 'i' };
      char regType = regTypes[intVal/8];
      char regIdx = '0' + (intVal % 8);
      char tmp[] = { '{', regType, regIdx, '}', 0 };
      std::string newConstraint = std::string(tmp);
      return TargetLowering::getRegForInlineAsmConstraint(TRI, newConstraint,
                                                          VT);
    }
    if (name.substr(0, 1).equals("f") &&
        !name.substr(1).getAsInteger(10, intVal) && intVal <= 63) {
      std::string newConstraint;

      if (VT == MVT::f32 || VT == MVT::Other) {
        newConstraint = "{f" + utostr(intVal) + "}";
      } else if (VT == MVT::f64 && (intVal % 2 == 0)) {
        newConstraint = "{d" + utostr(intVal / 2) + "}";
      } else if (VT == MVT::f128 && (intVal % 4 == 0)) {
        newConstraint = "{q" + utostr(intVal / 4) + "}";
      } else {
        return std::make_pair(0U, nullptr);
      }
      return TargetLowering::getRegForInlineAsmConstraint(TRI, newConstraint,
                                                          VT);
    }
  }

  return TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);
}

bool
SimTargetLowering::isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The Sim target isn't yet aware of offsets.
  return false;
}

void SimTargetLowering::ReplaceNodeResults(SDNode *N,
                                             SmallVectorImpl<SDValue>& Results,
                                             SelectionDAG &DAG) const {

  SDLoc dl(N);

  RTLIB::Libcall libCall = RTLIB::UNKNOWN_LIBCALL;

  switch (N->getOpcode()) {
  default:
    llvm_unreachable("Do not know how to custom type legalize this operation!");

  case ISD::FP_TO_SINT:
  case ISD::FP_TO_UINT:
    // Custom lower only if it involves f128 or i64.
    if (N->getOperand(0).getValueType() != MVT::f128
        || N->getValueType(0) != MVT::i64)
      return;
    libCall = ((N->getOpcode() == ISD::FP_TO_SINT)
               ? RTLIB::FPTOSINT_F128_I64
               : RTLIB::FPTOUINT_F128_I64);

    Results.push_back(LowerF128Op(SDValue(N, 0),
                                  DAG,
                                  getLibcallName(libCall),
                                  1));
    return;
  case ISD::READCYCLECOUNTER: {
    SDValue Lo = DAG.getCopyFromReg(N->getOperand(0), dl, SIM::ASR23, MVT::i32);
    SDValue Hi = DAG.getCopyFromReg(Lo, dl, SIM::G0, MVT::i32);
    SDValue Ops[] = { Lo, Hi };
    SDValue Pair = DAG.getNode(ISD::BUILD_PAIR, dl, MVT::i64, Ops);
    Results.push_back(Pair);
    Results.push_back(N->getOperand(0));
    return;
  }
  case ISD::SINT_TO_FP:
  case ISD::UINT_TO_FP:
    // Custom lower only if it involves f128 or i64.
    if (N->getValueType(0) != MVT::f128
        || N->getOperand(0).getValueType() != MVT::i64)
      return;

    libCall = ((N->getOpcode() == ISD::SINT_TO_FP)
               ? RTLIB::SINTTOFP_I64_F128
               : RTLIB::UINTTOFP_I64_F128);

    Results.push_back(LowerF128Op(SDValue(N, 0),
                                  DAG,
                                  getLibcallName(libCall),
                                  1));
    return;
  case ISD::LOAD: {
    LoadSDNode *Ld = cast<LoadSDNode>(N);
    // Custom handling only for i64: turn i64 load into a v2i32 load,
    // and a bitcast.
    if (Ld->getValueType(0) != MVT::i64 || Ld->getMemoryVT() != MVT::i64)
      return;

    SDLoc dl(N);
    SDValue LoadRes = DAG.getExtLoad(
        Ld->getExtensionType(), dl, MVT::v2i32, Ld->getChain(),
        Ld->getBasePtr(), Ld->getPointerInfo(), MVT::v2i32,
        Ld->getOriginalAlign(), Ld->getMemOperand()->getFlags(),
        Ld->getAAInfo());

    SDValue Res = DAG.getNode(ISD::BITCAST, dl, MVT::i64, LoadRes);
    Results.push_back(Res);
    Results.push_back(LoadRes.getValue(1));
    return;
  }
  }
}

// Override to enable LOAD_STACK_GUARD lowering on Linux.
bool SimTargetLowering::useLoadStackGuardNode() const {
  // TODO: remove
  // if (!Subtarget->isTargetLinux())
  return TargetLowering::useLoadStackGuardNode();
  // return true;
}

// Override to disable global variable loading on Linux.
void SimTargetLowering::insertSSPDeclarations(Module &M) const {
  // if (!Subtarget->isTargetLinux())
  return TargetLowering::insertSSPDeclarations(M);
}
