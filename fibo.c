#include <stdio.h>
int fibo(int x);

int main()
{
  printf("fib( 30 ) = %d\n", fibo(30));
}

int fibo(int x)
{
  if (x == 0)
    return 0;
  if (x <= 2)
    return 1;
  return fibo(x - 1) + fibo(x - 2);
}