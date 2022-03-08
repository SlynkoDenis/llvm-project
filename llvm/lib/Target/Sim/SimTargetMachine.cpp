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
  std::string Ret = "E";
  Ret += "-m:e";

  // Some ABIs have 32bit pointers.
  Ret += "-p:32:32";

  // Alignments for 64 bit integers.
  Ret += "-i64:64";

  Ret += "-f128:64-n32";

  Ret += "-S64";

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

const SimSubtarget *
SimTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString().str() : TargetCPU;
  std::string FS =
      FSAttr.isValid() ? FSAttr.getValueAsString().str() : TargetFS;

  // FIXME: This is related to the code below to reset the target options,
  // we need to know whether or not the soft float flag is set on the
  // function, so we can enable it as a subtarget feature.
  bool softFloat = F.getFnAttribute("use-soft-float").getValueAsBool();

  if (softFloat)
    FS += FS.empty() ? "+soft-float" : ",+soft-float";

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = std::make_unique<SimSubtarget>(TargetTriple, CPU, FS, *this);
  }
  return I.get();
}
}   // llvm

extern "C" void LLVMInitializeSimTarget() {
  // Register the target.
  llvm::RegisterTargetMachine<llvm::SimTargetMachine> X(getTheSimTarget());
}
