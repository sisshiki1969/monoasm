def fibo(x)
    if x == 0 then return 0 end
    if x <= 2 then return 1 end
    return fibo(x-1) + fibo(x-2)
end
now = Time.now
answer = fibo(30)
puts "#{answer} #{(Time.now - now) * 1000} ms"