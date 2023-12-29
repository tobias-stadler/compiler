let riscv = Arch {
let X0 = Reg {
  let noLiveness = bool { true }
  let alias = token { "zero" }
}
let X1 = Reg {
  let alias = token { "ra" }
}
let X2 = Reg {
  let alias = token { "sp" }
}
let X3 = Reg {
  let alias = token { "gp" }
}
let X4 = Reg {
  let alias = token { "tp" }
}
let X5 = Reg {
  let alias = token { "t0" }
}
let X6 = Reg {
  let alias = token { "t1" }
}
let X7 = Reg {
  let alias = token { "t2" }
}
let X8 = Reg {
  let alias = token { "s0" }
}
let X9 = Reg {
  let alias = token { "s1" }
}
let X10 = Reg {
  let alias = token { "a0" }
}
let X11 = Reg {
  let alias = token { "a1" }
}
let X12 = Reg {
  let alias = token { "a2" }
}
let X13 = Reg {
  let alias = token { "a3" }
}
let X14 = Reg {
  let alias = token { "a4" }
}
let X15 = Reg {
  let alias = token { "a5" }
}
let X16 = Reg {
  let alias = token { "a6" }
}
let X17 = Reg {
  let alias = token { "a7" }
}
let X18 = Reg {
  let alias = token { "s2" }
}
let X19 = Reg {
  let alias = token { "s3" }
}
let X20 = Reg {
  let alias = token { "s4" }
}
let X21 = Reg {
  let alias = token { "s5" }
}
let X22 = Reg {
  let alias = token { "s6" }
}
let X23 = Reg {
  let alias = token { "s7" }
}
let X24 = Reg {
  let alias = token { "s8" }
}
let X25 = Reg {
  let alias = token { "s9" }
}
let X26 = Reg {
  let alias = token { "s10" }
}
let X27 = Reg {
  let alias = token { "s11" }
}
let X28 = Reg {
  let alias = token { "t3" }
}
let X29 = Reg {
  let alias = token { "t4" }
}
let X30 = Reg {
  let alias = token { "t5" }
}
let X31 = Reg {
  let alias = token { "t6" }
}

let GPR = RegClass {
  let regs = dsl_list { X5 X6 X7 X28 X29 X30 X31 }
}

let LUI = Instr {
  let alias = token { "lui" }
}
let AUIPC = Instr {
  let alias = token { "auipc" }
}

let JAL = Instr {
  let alias = token { "jal" }
}
let JALR = Instr {
  let alias = token { "jalr" }
}
let BEQ = Instr {
  let alias = token { "beq" }
}
let BNE = Instr {
  let alias = token { "bne" }
}
let BLT = Instr {
  let alias = token { "blt" }
}
let BGE = Instr {
  let alias = token { "bge" }
}
let BLTU = Instr {
  let alias = token { "bltu" }
}
let BGEU = Instr {
  let alias = token { "bgeu" }
}

let SLTI = Instr {
  let alias = token { "slti" }
}
let SLTIU = Instr {
  let alias = token { "sltiu" }
}
let SLT = Instr {
  let alias = token { "slt" }
}
let SLTU = Instr {
  let alias = token { "sltu" }
}

let ADDI = Instr {
  let alias = token { "addi" }
}
let ORI = Instr {
  let alias = token { "ori" }
}
let ANDI = Instr {
  let alias = token { "andi" }
}
let XORI = Instr {
  let alias = token { "xori" }
}
let SLLI = Instr {
  let alias = token { "slli" }
}
let SRLI = Instr {
  let alias = token { "srli" }
}
let SRAI = Instr {
  let alias = token { "srai" }
}

let ADD = Instr {
  let alias = token { "add" }
}
let SUB = Instr {
  let alias = token { "sub" }
}
let OR = Instr {
  let alias = token { "or" }
}
let AND = Instr {
  let alias = token { "and" }
}
let XOR = Instr {
  let alias = token { "xor" }
}
let SLL = Instr {
  let alias = token { "sll" }
}
let SRL = Instr {
  let alias = token { "srl" }
}
let SRA = Instr {
  let alias = token { "sra" }
}

let LB = Instr {
  let alias = token { "lb" }
}
let LH = Instr {
  let alias = token { "lh" }
}
let LW = Instr {
  let alias = token { "lw" }
}
let LBU = Instr {
  let alias = token { "lbu" }
}
let LHU = Instr {
  let alias = token { "lhu" }
}
let SB = Instr {
  let alias = token { "sb" }
}
let SH = Instr {
  let alias = token { "sh" }
}
let SW = Instr {
  let alias = token { "sw" }
}

let PSEUDO_CALL = Instr {
  let alias = token { "call" }
}
let PSEUDO_LA = Instr {
  let alias = token { "la" }
}
let PSEUDO_LI = Instr {
  let alias = token { "li" }
}
}
