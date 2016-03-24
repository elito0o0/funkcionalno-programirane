main=do
 print (memb 3 [2,3,4,5])
 print (intersect [2,3,4] [2,4,5,6])
 print (union [2,3,4] [2,4,5,6])
 print (f1 (-2) (f))
 print ((f2 '*') 1 2)
 print((\ a b -> a+b+2) 1 2 )
memb::Int->[Int]->Bool
memb _ []=False
memb a (x:xs)=if(a==x) then True else (memb a xs)

intersect::[Int]->[Int]->[Int]
intersect [] _ =[]
intertsect (x:xs) ys =if(memb x ys) then x:(intersect xs ys) else (intersect xs ys)

union::[Int]->[Int]->[Int]
union (x:xs) ys =if(memb x ys) then (union xs ys) else x:(union xs ys)
f::Int->Int

f 0 = -1
f x =2*x
f1::Int->(Int->Int)->Int
f1 x g = g (x+2)

f2::Char->(Int->Int->Int)
f2 c= if(c=='*')then mult else mysum
mysum::Int->Int->Int
mysum a b = a+b
mult:: Int->Int->Int
mult a b= a*b

