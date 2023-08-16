const now = performance.now();
const answer = fibo(30)
const time = (performance.now() - now).toFixed(5);
console.log(`${answer} ${time} ms`)


function fibo(x) {
  if (x == 0)  return 0 
  if (x <= 2)  return 1 
  return fibo(x-1) + fibo(x-2)
}