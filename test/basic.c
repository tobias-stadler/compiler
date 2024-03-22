static int global_x;

int do_sth(int a, int b) { return a + b; }

struct goofy {
  char x;
  int y;
  struct goofy *p;
};
typedef enum { A, ASDF = 3, B, C } b;

int main() {
  // int single, line, comment;

  /* int multi, line, comment;
   int x;
  * */
  int x, y, z;
  x = 0;
  int *p;
  p = &x;
  y = 2;
  z = x + y + 5;
  *p = do_sth(x, y);

  switch (x) {
  case 1:
  case 2:
    x = x + 3;
  default:
    x = 0;
  }

  struct goofy nah = (struct goofy){.x = 3};
  int n2 = sizeof(int) * x;

  do
    x += -0xff;
  while (x);

  struct goofy *(*gf)(int);
  b bv;

  struct goofy s;
  s.x = 69;
  s.x++;
  (&s)->y = 42;
#define C A + B
#define A 69
#define B 42
#define FUNC(a,b) a + b

return 0
#ifdef A
  + 1
#ifndef B
  + 2
#else
  + 3
#endif
  + 4
#else
  + 5
#ifdef B
  + 6
#endif
  + 7
#endif
  ;

#undef B
  return FUNC  ( A , B );
}
