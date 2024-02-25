let incIR = include {"ir/IR.dsl"}
let incArch = include {"riscv/Arch.dsl"}

using {
  incArch
}

let ArithImmPat = Template {
ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
    IN def(%arith,i32) %lhs %c;
  }
  if {
    $isLegalImm($ #imm $)$
  }
  emit {
    riscv::OUT def(%arith,i32) %lhs #imm;
  }
}
ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
    IN def(%arith,i32) %c %lhs;
  }
  if {
    $isLegalImm($ #imm $)$
  }
  emit {
    riscv::OUT def(%arith,i32) %lhs #imm;
  }
}
}

let ArithPat = Template {
ir_pat {
  match {
    IN def(%arith,i32) %lhs %rhs;
  }
  emit {
    riscv::OUT def(%arith,i32) %lhs %rhs;
  }
}
}

let ShiftImmPat = Template {
ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
    IN def(%arith,i32) %lhs %c;
  }
  if {
    $isLegalImm($ #imm $)$
  }
  emit {
    riscv::OUT def(%arith,i32) %lhs #imm;
  }
}
}

let CondBrPat = Template {
ir_pat {
  match {
    CMP def(%cmp,i32) IN %lhs %rhs;
    BR_COND %cmp %b1 %b2;
  }
  emit {
    riscv::OUT %lhs %rhs %b1 %b2;
  }
}
}

let CmpPat = Template {
ir_pat {
  match {
    CMP def(%cmp,i32) IN;
  }
  emit {
    riscv::OUT def(%cmp,i32) %lhs %rhs;
  }
}
}
let CmpImmPat = Template {
ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
    CMP def(%cmp,i32) IN;
  }
  if {
    #imm $.imm32() != 0$
    $&&$
    $isLegalImm($ #imm $)$
  }
  emit {
    riscv::OUT def(%cmp,i32) %lhs #imm;
  }
}
}
let CmpNegPat = Template {
ir_pat {
  match {
    CMP def(%cmp,i32) IN;
  }
  emit {
    riscv::OUT def(%set,i32) %lhs %rhs;
    riscv::XORI def(%cmp,i32) %set imm32(1);
  }
}
}
let CmpNegImmPat = Template {
ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
    CMP def(%cmp,i32) IN;
  }
  if {
    #imm $.imm32() != 0$
    $&&$
    $isLegalImm($ #imm $)$
  }
  emit {
    riscv::OUT def(%set,i32) %lhs #imm;
    riscv::XORI def(%cmp,i32) %set imm32(1);
  }
}
}

let TruncExtPat = Template {
ir_pat {
  match {
    TRUNC def(%tr,_) %src;
    IN def(%dst,i32) %tr;
  }
  if {
    %src $.ssaDefType() == IntSSAType::get(32)$
  }
  emit {
    riscv::SLLI def(%sl,i32) %src imm32($selectExtShiftBits($ %tr $)$);
    riscv::OUT def(%dst,i32) %sl imm32($selectExtShiftBits($ %tr $)$);
  }
}
}

let ArtifactCombinePat = Template {
ir_pat {
  match {
    ART1 def(%a1,_) %src;
    ART2 def(%a2,_) %a1;
  }
  emit {
    OUT def(%a2,%a2$.ssaDefType()$) %src;
  }
}
}

let preISelExpansion = IRPatExecutor {
ir_pat {
  match {
    LOAD def(%dst,i32) %addr;
  }
  emit {
    riscv::LW def(%dst,i32) %addr imm32(0);
  }
}
ir_pat {
  match {
    LOAD def(%dst,i16) %addr;
  }
  emit {
    riscv::LH def(%ld,i32) %addr imm32(0);
    TRUNC def(%dst,i16) %ld;
  }
}
ir_pat {
  match {
    LOAD def(%dst,i8) %addr;
  }
  emit {
    riscv::LB def(%ld,i32) %addr imm32(0);
    TRUNC def(%dst,i8) %ld;
  }
}
}

let preISelCombine = IRPatExecutor {
!ArtifactCombinePat {
  let ART1 = token {EXT_Z}
  let ART2 = token {EXT_Z}
  let OUT = token {EXT_Z}
}
!ArtifactCombinePat {
  let ART1 = token {EXT_S}
  let ART2 = token {EXT_S}
  let OUT = token {EXT_S}
}
!ArtifactCombinePat {
  let ART1 = token {EXT_A}
  let ART2 = token {EXT_A}
  let OUT = token {EXT_A}
}
!ArtifactCombinePat {
  let ART1 = token {TRUNC}
  let ART2 = token {TRUNC}
  let OUT = token {TRUNC}
}
!ArtifactCombinePat {
  let ART1 = token {EXT_Z}
  let ART2 = token {EXT_S}
  let OUT = token {EXT_Z}
}
ir_pat {
  match {
    TRUNC def(%a1,_) %src;
    EXT_A def(%a2,_) %a1;
  }
  if {
    %src $.ssaDefType() == $ %a2 $.ssaDefType()$
  }
  apply {
    %a2 $.ssaDef().replaceAllUses($ %src $)$
  }
}
ir_pat {
  match {
    CMP def(%src,i32);
    TRUNC def(%a1,i1) %src;
    EXT_Z def(%a2,i32) %a1;
  }
  apply {
    %a2 $.ssaDef().replaceAllUses($ %src $)$
  }
}
}

let isel = IRPatExecutor {
!TruncExtPat {
  let IN = token {EXT_Z}
  let OUT = token {SRLI}
}
!TruncExtPat {
  let IN = token {EXT_S}
  let OUT = token {SRAI}
}

ir_pat {
  match {
    CONST_INT def(%c,i32) imm32(0);
  }
  emit {
    COPY def(%c,i32) riscv::X0;
  }
}
ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
  }
  emit {
    riscv::PSEUDO_LI def(%c,i32) #imm;
  }
}

ir_pat {
  match {
    REF_OTHERSSADEF def(%gaddr,i32) %g;
  }
  if {
    %g $.ssaDefOther().isGlobal()$
  }
  emit {
    riscv::PSEUDO_LA def(%gaddr,i32) %g;
  }
}

ir_pat {
  match {
    BR %b;
  }
  emit {
    riscv::JAL riscv::X0 %b;
  }
}

!CmpPat {
  let IN = token {lt %lhs %rhs}
  let OUT = token {SLT}
}
!CmpImmPat {
  let IN = token {lt %lhs %c}
  let OUT = token {SLTI}
}
!CmpPat {
  let IN = token {ltu %lhs %rhs}
  let OUT = token {SLTU}
}
!CmpImmPat {
  let IN = token {ltu %lhs %c}
  let OUT = token {SLTIU}
}
!CmpNegPat {
  let IN = token {ge %lhs %rhs}
  let OUT = token {SLT}
}
!CmpNegImmPat {
  let IN = token {ge %lhs %c}
  let OUT = token {SLTI}
}
!CmpNegPat {
  let IN = token {geu %lhs %rhs}
  let OUT = token {SLTU}
}
!CmpNegImmPat {
  let IN = token {geu %lhs %c}
  let OUT = token {SLTIU}
}

ir_pat {
  match {
    CONST_INT def(%c,i32) imm32(0);
    CMP def(%cmp,i32) eq %lhs %c;
  }
  emit {
    riscv::SLTIU def(%cmp,i32) %lhs imm32(1);
  }
}
ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
    CMP def(%cmp,i32) eq %lhs %c;
  }
  if {
    $isLegalImmNegated($ #imm $)$
  }
  emit {
    riscv::ADDI def(%sub,i32) %lhs imm32($-$#imm$.imm32()$);
    riscv::SLTIU def(%cmp,i32) %sub imm32(1);
  }
}
ir_pat {
  match {
    CMP def(%cmp,i32) eq %lhs %rhs;
  }
  emit {
    riscv::SUB def(%sub,i32) %lhs %rhs;
    riscv::SLTIU def(%cmp,i32) %sub imm32(1);
  }
}

ir_pat {
  match {
    CONST_INT def(%c,i32) imm32(0);
    CMP def(%cmp,i32) ne %lhs %c;
  }
  emit {
    riscv::SLTU def(%cmp,i32) riscv::X0 %lhs;
  }
}
ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
    CMP def(%cmp,i32) ne %lhs %c;
  }
  if {
    $isLegalImmNegated($ #imm $)$
  }
  emit {
    riscv::ADDI def(%sub,i32) %lhs imm32($-$#imm$.imm32()$);
    riscv::SLTU def(%cmp,i32) riscv::X0 %lhs;
  }
}
ir_pat {
  match {
    CMP def(%cmp,i32) ne %lhs %rhs;
  }
  emit {
    riscv::SUB def(%sub,i32) %lhs %rhs;
    riscv::SLTU def(%cmp,i32) riscv::X0 %lhs;
  }
}

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
!CondBrPat {
  let IN = token {ge}
  let OUT = token {BGE}
}
!CondBrPat {
  let IN = token {geu}
  let OUT = token {BGEU}
}

ir_pat {
  match {
    CONST_INT def(%c,i32) #imm;
    SUB def(%arith,i32) %lhs %c;
  }
  if {
    $isLegalImmNegated($ #imm $)$
  }
  emit {
    riscv::ADDI def(%arith,i32) %lhs imm32($-$#imm$.imm32()$);
  }
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
!ShiftImmPat {
  let IN = token {SL_L}
  let OUT = token {SLLI}
}
!ShiftImmPat {
  let IN = token {SR_L}
  let OUT = token {SRLI}
}
!ShiftImmPat {
  let IN = token {SR_A}
  let OUT = token {SRAI}
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

ir_pat {
  match {
    STORE %src %addr;
  }
  if {
    %src $.ssaDefType() == IntSSAType::get(32)$
  }
  emit {
    riscv::SW %src %addr imm32(0);
  }
}
ir_pat {
  match {
    TRUNC def(%tr,i16) %src;
    STORE %tr %addr;
  }
  emit {
    riscv::SH %src %addr imm32(0);
  }
}
ir_pat {
  match {
    TRUNC def(%tr,i8) %src;
    STORE %tr %addr;
  }
  emit {
    riscv::SB %src %addr imm32(0);
  }
}

}
