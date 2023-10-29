int main() {

  short y = 2 ^ ((300 + 500) >> 1);
  if (y > 0) {
    return y;
  } else {
    return y + 1;
  }

  int x = y - 3;
  int z = 5 < 0;
  return z;
}
