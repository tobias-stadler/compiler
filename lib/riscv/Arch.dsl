let riscv = Arch {
let X0 = Register {}
let X1 = Register {}
let X2 = Register {}
let X3 = Register {}
let X4 = Register {}
let X5 = Register {}
let X6 = Register {}
let X7 = Register {}

let GPR32 = RegClass {
  let registers = dsl_list {x1 x2 x3 x4 x5 x6 x7}
}
let ADDI = Instr {}
let SUBI = Instr {}
}
