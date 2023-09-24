let RISCV = Architecture {
let x1 = Reg {}
let x2 = Reg {}
let registers = dsl_list {x1 x2 x3 x4 x5 x6}

let gpr32 = RegClass {
  dsl_list { x1,x2,x3,x4,x5 }
}

let ADDI = Instr {}

let selADDI = IRPattern {
  match {
    CONSTint def(%2,i32) #1;
    ADD def(%1,i32) %2 %3;
  }
  if {
      $validImmediate(#1)$
  } then {
    emit {
      ADDI def(%1,gpr32) %3 imm12($select12immediate(%2)$);
    }
  } else {

  }
}

let legalADD = {
  legal { i32 }
  by {intExt}
}

let isel = InstrSelector {
  selADDI
}
}
