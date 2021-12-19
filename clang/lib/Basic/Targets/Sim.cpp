//===--- Sim.cpp - Implement Sim target feature support ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements Sim TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "Sim.h"
#include "Targets.h"
#include "clang/Basic/MacroBuilder.h"
#include "llvm/ADT/StringSwitch.h"

using namespace clang;
using namespace clang::targets;

const char *const SimTargetInfo::GCCRegNames[] = {
    // Integer registers
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",  "r8",  "r9",  "r10",
    "r11", "r12", "r13", "r14", "r15",
};

ArrayRef<const char *> SimTargetInfo::getGCCRegNames() const {
  return llvm::makeArrayRef(GCCRegNames);
}

const TargetInfo::GCCRegAlias SimTargetInfo::GCCRegAliases[] = {
    {{"g0"}, "r0"},  {{"g1"}, "r1"},  {{"g2"}, "r2"},  {{"g3"}, "r3"},
    {{"g4"}, "r4"},  {{"g5"}, "r5"},  {{"g6"}, "r6"},  {{"g7"}, "r7"},
    {{"g8"}, "r8"},  {{"g9"}, "r9"},  {{"a0"}, "r10"}, {{"a1"}, "r11"},
    {{"a2"}, "r12"}, {{"a3"}, "r13"}, {{"a4"}, "r14"}, {{"a5"}, "r15"},
};

ArrayRef<TargetInfo::GCCRegAlias> SimTargetInfo::getGCCRegAliases() const {
  return llvm::makeArrayRef(GCCRegAliases);
}

void SimTargetInfo::getTargetDefines(const LangOptions &Opts,
                                       MacroBuilder &Builder) const {
  DefineStd(Builder, "sim", Opts);
  Builder.defineMacro("__ELF__");
}
