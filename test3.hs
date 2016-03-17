--ex::Double->Double->Double
--fact::Int->Int
fact n = if(n==0 || n==1) then 1 else (n*(fact (n-1)))
ex x n
   |(n==0)        =1
   |otherwise     =  (x^n)/(fromIntegral (fact n)) + (ex x (n-1)) 

sinx x n
  |(n==0)    = x
  |otherwise =((-1)^n)*x^(2*n+1)/(fromIntegral (fact (2*n+1))) + (sinx x (n-1))
main=do
 print (ex 3 10)
 print (sinx 1.57 3) 
