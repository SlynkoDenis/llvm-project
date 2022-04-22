//===-- SimSubtarget.cpp - Sim Subtarget Information ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the Sim specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "SimSubtarget.h"
#include "Sim.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;

#define DEBUG_TYPE "sim-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "SimGenSubtargetInfo.inc"

void SimSubtarget::anchor() { }

SimSubtarget &SimSubtarget::initializeSubtargetDependencies(StringRef CPU,
                                                            StringRef FS) {

  std::string CPUName = std::string(CPU);
  if (CPUName.empty()) {
    CPUName = "Sim";
  }
  // Parse features string.
  ParseSubtargetFeatures(CPUName, CPUName, FS);

  return *this;
}

SimSubtarget::SimSubtarget(const Triple &TT, const std::string &CPU,
                           const std::string &FS, const TargetMachine &TM)
    : SimGenSubtargetInfo(TT, CPU, /*TuneCPU*/ CPU, FS), TargetTriple(TT),
      InstrInfo(initializeSubtargetDependencies(CPU, FS)),
      TLInfo(TM, *this), FrameLowering(*this) {}

bool SimSubtarget::enableMachineScheduler() const {
  return true;
}
