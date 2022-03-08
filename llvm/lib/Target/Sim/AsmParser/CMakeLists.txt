add_llvm_component_library(LLVMSimAsmParser
  SimAsmParser.cpp

  LINK_COMPONENTS
  MC
  MCParser
  SimDesc

  ADD_TO_COMPONENT
  Sim
  )
