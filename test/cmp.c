int var_var(int a, int b) {
  _Bool x1 = a == b;
  _Bool x2 = a != b;
  _Bool x3 = a < b;
  _Bool x4 = a > b;
  _Bool x5 = a <= b;
  _Bool x6 = a >= b;
  return x1 & x2 & x3 & x4 & x5 & x6;
}
int var_const(int a) {
  _Bool x1 = a == 4;
  _Bool x2 = a != 4;
  _Bool x3 = a < 4;
  _Bool x4 = a > 4;
  _Bool x5 = a <= 4;
  _Bool x6 = a >= 4;
  return x1 & x2 & x3 & x4 & x5 & x6;
}
int const_var(int a) {
  _Bool x1 = 4 == a;
  _Bool x2 = 4 != a;
  _Bool x3 = 4 < a;
  _Bool x4 = 4 > a;
  _Bool x5 = 4 <= a;
  _Bool x6 = 4 >= a;
  return x1 & x2 & x3 & x4 & x5 & x6;
}
int var_0(int a) {
  _Bool x1 = a == 0;
  _Bool x2 = a != 0;
  _Bool x3 = a < 0;
  _Bool x4 = a > 0;
  _Bool x5 = a <= 0;
  _Bool x6 = a >= 0;
  return x1 & x2 & x3 & x4 & x5 & x6;
}
int zero_var(int a) {
  _Bool x1 = 0 == a;
  _Bool x2 = 0 != a;
  _Bool x3 = 0 < a;
  _Bool x4 = 0 > a;
  _Bool x5 = 0 <= a;
  _Bool x6 = 0 >= a;
  return x1 & x2 & x3 & x4 & x5 & x6;
}
