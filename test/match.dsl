let RISCV = Arch {

}
let bruh = IRPattern {
  match {
  CONST_INT def(%1,_) #1;
  ADD def(%_,_) %1 %1;
  }
  emit {
  }
}
