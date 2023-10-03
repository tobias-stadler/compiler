let incIR = include {"ir/IR.dsl"}
let incArch = include {"riscv/Arch.dsl"}

using {
  incArch
}

let ArithImmPat = Template {
ir_pat {
  match {
    CONST_INT def(%2,_) #imm;
    IN def(%1,i32) %2 %3;
  }
  if {
    $TCInt::canTrunc<12>($ #imm $.imm32())$
  }
  emit {
    riscv::OUT def(%1,i32) %3 #imm;
  }
}
ir_pat {
  match {
    CONST_INT def(%2,_) #imm;
    IN def(%1,i32) %3 %2;
  }
  if {
    $TCInt::canTrunc<12>($ #imm $.imm32())$
  }
  emit {
    riscv::OUT def(%1,i32) %3 #imm;
  }
}
}

let ArithPat = Template {
ir_pat {
  match {
    IN def(%1,i32) %2 %3;
  }
  emit {
    riscv::OUT def(%1,i32) %2 %3;
  }
}
}

let isel = InstrSelector {

!ArithImmPat {
  let IN = token {ADD}
  let OUT = token {ADDI}
}
!ArithImmPat {
  let IN = token {OR}
  let OUT = token {ORI}
}
!ArithImmPat {
  let IN = token {AND}
  let OUT = token {ANDI}
}
!ArithImmPat {
  let IN = token {XOR}
  let OUT = token {XORI}
}

!ArithPat {
  let IN = token {ADD}
  let OUT = token {ADD}
}
!ArithPat {
  let IN = token {SUB}
  let OUT = token {SUB}
}
!ArithPat {
  let IN = token {OR}
  let OUT = token {OR}
}
!ArithPat {
  let IN = token {AND}
  let OUT = token {AND}
}
!ArithPat {
  let IN = token {XOR}
  let OUT = token {XOR}
}
!ArithPat {
  let IN = token {SL_L}
  let OUT = token {SLL}
}
!ArithPat {
  let IN = token {SR_L}
  let OUT = token {SRL}
}
!ArithPat {
  let IN = token {SR_A}
  let OUT = token {SRA}
}
}
