main=do
 print (listlength [1,2,3,4,5])
 --print ( digits 123)
 --print (myreverse 123)
 --print (rev 451)
 --print (maxi 4 5 3)
 --print (f2 5 2 )
 print (myevensum [1,2,3,4,5,6])

digits n = if(n<10)then 1 else(1 + (digits (div n 10)))-- broq na cifrite v 4islo
myreverse n = if(n<10) then n else (mod n 10)*(10^(digits n-1))+(myreverse (div n 10))-- reverse
revHelper n result = if(n==0) then result else (revHelper (div n 10) (result*10 + (0+(mod n 10))))-- obra6tane na 4islo 4rez pome6tna funkciq
rev n = revHelper n 0
maxi a b c 
 |(a>=b)&&(a>=c)    = a
 |(b>=a) && (b>=c)  = b
 |otherwise         = c
f2 0 1 =10
f2 1 x =x+2
f2 _ 1 = -1
f2 x y = (x+y)*2

listlength []=0
listlength xs=1+listlength (tail xs) --daljina na spisak
--f [] =[1,2]
--f (x:y:xs) = (x:[y])
--f (x:xs) =x:xs
-- null (_:_) = False - proverka za prazen spisak
--null [] = True 
myevensum []=0
myevensum (x:xs)=if((mod x 2)==0)then (x + myevensum xs) else (myevensum xs)
