main=do
  print(countPalindromes [11..22] 0)
  print(countmin [1,2,1,1,5,3] )
  print(intersect [1,2,2] [2,2,2,3] [])
  print (list2primes (evenNumbers [5,4,7,6,8,9] []))
  
 --Zada4a 1
 
myreverse::Int->Int->Int
myreverse n m = if(n<10) then (m*10+n) else (myreverse (div n 10) (m*10+(mod n 10)))

isPalindrome::Int->Bool
isPalindrome n = if(n==(myreverse n 0)) then True else False

countPalindromes::[Int]->Int->Int
countPalindromes [] k = k
countPalindromes (x:xs) k
    |isPalindrome x  =(countPalindromes xs (k+1))
    |otherwise       =(countPalindromes xs k)

--Zada4a 2

countminHelper::[Int]->Int->[Int]->Int
countminHelper [] h (z:zs) = h
countminHelper (y:ys) h (z:zs)
   |y==minimum (z:zs)  =(countminHelper ys (h+1) (z:zs))
   |otherwise          =(countminHelper ys h (z:zs))

countmin::[Int]->Int
countmin []=0
countmin (y:ys) = (countminHelper (y:ys) 0 (y:ys))

--Zada4a 3

intersect::[Int]->[Int]->[Int]->[Int]
intersect [] _ zs =zs
intersect _ [] zs =zs
intersect (x:xs) (y:ys) zs
    |(elem x (y:ys))   =(intersect (xs) (y:ys) (zs++[x]))
    |otherwise         =(intersect (xs) (y:ys) (zs))

isPrime n = if(n <= 1) then False  else  (isPrime' 2 n) 
isPrime' current n = if(current == n) then True else( if( mod n current == 0) then False else (isPrime' (current + 1) n))

evenNumbers::[Int]->[Int]->[Int]
evenNumbers [] ys = ys
evenNumbers (x:xs) ys
  |(mod x 2)==0  =evenNumbers xs (ys++[x])
  |otherwise     =evenNumbers  xs ys

num::Int->[Int]
num s =[x|x<-[2..s],isPrime x]
list2primes::[Int]->[(Int,Int)]
list2primes ys = [(a,b)|y<-ys,a<-(num y),b<-(num y),a<=b,a+b==y]
