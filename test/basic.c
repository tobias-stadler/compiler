int do_sth(int a, int b) {
  return a + b;
}

int main() {
  int x, y, z;
  x = 0;
  int *p;
  p = &x;
  y = 2;
  z = x + y + 5;
  *p = do_sth(x, y);
  return 0;
}
