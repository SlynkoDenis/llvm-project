//===-- SimMCAsmInfo.h - Sim asm properties -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the SimMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SIM_MCTARGETDESC_SIMMCASMINFO_H
#define LLVM_LIB_TARGET_SIM_MCTARGETDESC_SIMMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

class SimMCAsmInfo : public MCAsmInfoELF {
  void anchor() override;

public:
  explicit SimMCAsmInfo(const Triple &TT);
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SIM_MCTARGETDESC_SIMMCASMINFO_H
