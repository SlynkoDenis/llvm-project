//===-- SimTargetMachine.cpp - Define TargetMachine for Sim -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements the info about Sim target spec.
//
//===----------------------------------------------------------------------===//

#include "Sim.h"
#include "SimTargetMachine.h"
#include "SimSubtarget.h"
#include "SimTargetObjectFile.h"
#include "TargetInfo/SimTargetInfo.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"
using namespace llvm;

// #define DEBUG_TYPE "Sim"

static std::string computeDataLayout() {
  // little endian
  std::string Ret = "e-m:e";

  // Some ABIs have 32bit pointers.
  Ret += "-p:32:32";

  // Alignments for 1/8/16/32 bit integers.
  Ret += "-i1:8:32";
  Ret += "-i8:8:32";
  Ret += "-i16:16:32";
  Ret += "-i32:32:32";

  // Break 64 bit integers into two
  Ret += "-i64:32";

  Ret += "-f32:32:32";
  Ret += "-f64:32";

  Ret += "-n32";

  return Ret;
}

static Reloc::Model getEffectiveSimRelocModel(Optional<Reloc::Model> RM) {
  return RM.getValueOr(Reloc::Static);
}

static CodeModel::Model getEffectiveSimCodeModel() {
  return CodeModel::Small;
}

namespace llvm {

SimTargetMachine::SimTargetMachine(
    const Target &T, const Triple &TT, StringRef CPU, StringRef FS,
    const TargetOptions &Options, Optional<Reloc::Model> RM,
    Optional<CodeModel::Model> CM, CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T, computeDataLayout(), TT, CPU, FS, Options,
                        getEffectiveSimRelocModel(RM),
                        getEffectiveSimCodeModel(),
                        OL),
      TLOF(std::make_unique<SimTargetObjectFile>()),
      Subtarget(TT, std::string(CPU), std::string(FS), *this) {
  initAsmInfo();
}
}   // llvm

extern "C" void LLVMInitializeSimTarget() {
  // Register the target.
  llvm::RegisterTargetMachine<llvm::SimTargetMachine> X(getTheSimTarget());
}
