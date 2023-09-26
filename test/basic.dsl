let riscv = Arch {
let X0 = Reg {}
let X1 = Reg {}
let X2 = Reg {}
let X3 = Reg {}
let X4 = Reg {}
let X5 = Reg {}
let X6 = Reg {}
let X7 = Reg {}

let gpr32 = RegClass {
  let registers = dsl_list {x1 x2 x3 x4 x5 x6 x7}
}
let ADD = Instr {}
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

let isel = InstrSelector {
  selADDI
}
}
