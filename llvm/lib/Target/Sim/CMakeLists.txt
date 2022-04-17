add_llvm_component_group(Sim HAS_JIT)

set(LLVM_TARGET_DEFINITIONS Sim.td)

# Generate SimGenRegisterInfo.inc and SimGenInstrInfo.inc which included by
#  your hand code C++ files.
# SimGenRegisterInfo.inc came from SimRegisterInfo.td, SimGenInstrInfo.inc
#  came from SimInstrInfo.td.
tablegen(LLVM SimGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM SimGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM SimGenCallingConv.inc -gen-callingconv)
tablegen(LLVM SimGenDAGISel.inc -gen-dag-isel)
# tablegen(LLVM SimGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM SimGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM SimGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM SimGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM SimGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM SimGenMCPseudoLowering.inc -gen-pseudo-lowering)

# SimCommonTableGen must be defined
add_public_tablegen_target(SimCommonTableGen)

# SimCodeGen should match with LLVMBuild.txt SimCodeGen
add_llvm_target(SimCodeGen
  SimTargetMachine.cpp
  SimTargetObjectFile.cpp
  SimMachineFunctionInfo.cpp
  SimFrameLowering.cpp
  SimSubtarget.cpp
  SimISelLowering.cpp
  SimAsmPrinter.cpp
  SimInstrInfo.cpp
  SimRegisterInfo.cpp
  SimISelDAGToDAG.cpp
  Sim.cpp

  # TODO: remove
  # Analysis
  # GlobalISel
  LINK_COMPONENTS
  AsmPrinter
  CodeGen
  Core
  MC
  SimDesc
  SimInfo
  SelectionDAG
  Support
  Target

  ADD_TO_COMPONENT
  Sim
  )

# add_subdirectory(AsmParser)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
