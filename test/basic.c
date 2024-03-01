static int global_x;

int do_sth(int a, int b) { return a + b; }

struct goofy {
  char x;
  int y;
  struct goofy *p;
};
typedef enum {
  A,
  ASDF = 3,
  B,
  C
} b;

int main() {
  int x, y, z;
  x = 0;
  int *p;
  p = &x;
  y = 2;
  z = x + y + 5;
  *p = do_sth(x, y);

  struct goofy *(*gf)(int);

  b bv;

  struct goofy s;
  s.x = 69;
  s.y = 42;
  return 0;
}
