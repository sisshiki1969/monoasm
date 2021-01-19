console.log(`fib( 40 ) = ${fibo(40)}`)

function fibo(x) {
  if (x == 0)  return 0 
  if (x == 1)  return 1 
  return fibo(x-1) + fibo(x-2)
}