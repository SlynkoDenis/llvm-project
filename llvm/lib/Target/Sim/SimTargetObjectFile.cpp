//===-- RISCVTargetObjectFile.cpp - RISCV Object Info -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SimTargetObjectFile.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"

using namespace llvm;

void SimTargetObjectFile::Initialize(MCContext &Ctx,
                                     const TargetMachine &TM) {
    TargetLoweringObjectFileELF::Initialize(Ctx, TM);

    SmallDataSection = getContext().getELFSection(
                       ".sdata", ELF::SHT_PROGBITS, ELF::SHF_WRITE | ELF::SHF_ALLOC);
    SmallBSSSection = getContext().getELFSection(".sbss", ELF::SHT_NOBITS,
                                                 ELF::SHF_WRITE | ELF::SHF_ALLOC);
}
