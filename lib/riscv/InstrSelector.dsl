let incIR = include {"ir/IR.dsl"}
let incArch = include {"riscv/Arch.dsl"}

using {
  incIR
  incArch
}

let isel = InstrSelector {
ir_pat {
  match {
    CONST_INT def(%2,_) #imm;
    ADD def(%1,i32) %2 %3;
  }
  if {
    $TCInt::canTrunc<12>($ #imm $.imm32())$
  }
  emit {
    riscv::ADDI def(%1,i32) %3 #imm;
  }
}
ir_pat {
  match {
    CONST_INT def(%2,_) #imm;
    SUB def(%1,i32) %2 %3;
  }
  if {
    $TCInt::canTrunc<12>($ #imm $.imm32())$
  }
  emit {
    riscv::SUBI def(%1,i32) %3 #imm;
  }
}
}
