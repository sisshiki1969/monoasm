#include <stdio.h>
int fibo(int x);

int main()
{
  printf("fib( 40 ) = %d\n", fibo(40));
}

int fibo(int x)
{
  if (x == 0)
    return 0;
  if (x == 1)
    return 1;
  return fibo(x - 1) + fibo(x - 2);
}