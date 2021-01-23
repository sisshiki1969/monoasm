def fibo(x)
    if x == 0 then return 0 end
    if x == 1 then return 1 end
    return fibo(x-1) + fibo(x-2)
end
fibo(40)