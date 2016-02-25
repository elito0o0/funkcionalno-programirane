main=do
  --print (fib 4)
  print (fib 101)
--fib n=if(n==0 || n==1) then 1 else fib(n-1)+fib(n-2)

fib n = myfib n 1 1
myfib n x y = if(n<=1) then y else  (myfib(n-1) y (x+y))
main=do
  --print"aaa"
  --print (1 * 5)
  --print (4**0.5)
 -- print (div 15 6)
 --print (mod 15 6)
 --print (fact 5)
 --print (sumab 5 4)
fact n=if(n==0) then 1 else (n*(fact(n-1)))

sumab a b=a+b

mymin a b=if(a<b) then (a) else b

myfunc a b = ((a*a)+(b*b))/2
