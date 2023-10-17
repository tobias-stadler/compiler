let incIR = include {"ir/IR.dsl"}
let incArch = include {"riscv/Arch.dsl"}

using {
  incArch
}

let ArithImmPat = Template {
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

let CondBrPat = Template {
ir_pat {
  match {
    CMP def(%cmp,i1) IN %lhs %rhs;
    BR_COND %cmp %b1 %b2;
  }
  emit {
    riscv::OUT %lhs %rhs %b1 %b2;
  }
}
}
let CondBrPatCommute = Template {
ir_pat {
  match {
    CMP def(%cmp,i1) IN %lhs %rhs;
    BR_COND %cmp %b1 %b2;
  }
  emit {
    riscv::OUT %rhs %lhs %b1 %b2;
  }
}
}

let isel = InstrSelector {

!CondBrPat {
  let IN = token {eq}
  let OUT = token {BEQ}
}
!CondBrPat {
  let IN = token {ne}
  let OUT = token {BNE}
}
!CondBrPat {
  let IN = token {lt}
  let OUT = token {BLT}
}
!CondBrPat {
  let IN = token {ltu}
  let OUT = token {BLTU}
}
!CondBrPatCommute {
  let IN = token {le}
  let OUT = token {BGE}
}
!CondBrPatCommute {
  let IN = token {leu}
  let OUT = token {BGEU}
}
!CondBrPat {
  let IN = token {ge}
  let OUT = token {BGE}
}
!CondBrPat {
  let IN = token {geu}
  let OUT = token {BGEU}
}
!CondBrPatCommute {
  let IN = token {gt}
  let OUT = token {BLT}
}
!CondBrPatCommute {
  let IN = token {gtu}
  let OUT = token {BLTU}
}

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
