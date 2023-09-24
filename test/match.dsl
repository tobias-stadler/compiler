let RISCV = Arch {

}
let bruh = IRPattern {
  match {
  ADD def(%1,i16) %2 %3;
  }
  emit {
  ADD def(%1,i32) %2 %3;
  }
}
