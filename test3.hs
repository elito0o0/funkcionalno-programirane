fact::Int->Int
fact n = if(n==0 || n==1) then 1 else (n*(fact (n-1)))
ex  ::Double->Int->Double
ex x n
   |(n==0)        =1
   |otherwise     =  (x^n)/(fromIntegral (fact n)) + (ex x (n-1)) 
sinx::Double->Int->Double
sinx x n
  |(n==0)    = x
  |otherwise =((-1)^n)*x^(2*n+1)/(fromIntegral (fact (2*n+1))) + (sinx x (n-1))
memb :: Int->[t]->t
memb 0 (x:_)=x
memb n (_:xs)=memb (n-1) xs
--xs->[1,2,3] \ (x:xs)->1:[2,3] \ (x:y:xs)->1:2:[3]\(x:y:z:xs)->1:2:3:[]
remo::Int->[Int]->[Int]
remo _ []=[]
remo a (x:xs)
  |a==x       =xs
  |otherwise  =x:(remo a xs)
myrev::[t]->[t]
myrev []=[]
myrev (x:xs)=(myrev xs)++[x]
main=do
 print (ex 3 10)
 print (sinx 1.57 3) 
 print (memb 2 [1,2,3,4,5,6])
 print (remo 2 [1,2,3,4,5,6])
 print (myrev [1..8])
