//===-- SimTargetInfo.cpp - Sim Target Implementation -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Sim.h"
#include "SimTargetInfo.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

Target &llvm::getTheSimTarget() {
  static Target TheSimTarget;
  return TheSimTarget;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSimTargetInfo() {
  RegisterTarget<Triple::sim, /*HasJIT=*/false> X(
    getTheSimTarget(),
    "Sim",
    "Sim (32-bit simulator arch)", "Sim"
  );
}
