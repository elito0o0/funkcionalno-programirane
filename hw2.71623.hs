main=do
  print (convert 173 8 10)
  print (myPrimeReorder [2,3,4,5,6])
  
--Zadacha 1

--convertHelper1 e za preobrazuvane v 10 broina sistema
convertHelper1::Int->Int->Int->Int->Int->Int
convertHelper1 number pow res k n  
   |number==0    =res
   |otherwise    =(convertHelper1 (div number 10) (pow+1) (res+ (mod number 10)*(k)^(pow)) k n)

--convertHelper2 e za preobrazuvane ot 10 broina sistema v n-ta broina sistema
convertHelper2::Int->Int->Int->Int->Int->Int
convertHelper2 number i res k n
   |number==0   =res
   |otherwise   =(convertHelper2 (div number n) (i*10) (res+(mod number n)*i) k n)
   
--ako k/=10 vzemame convertHelper2 kato na mqstoto na promenlivata(number) slagame rezultata ot convertHelper1
convert::Int->Int->Int->Int
convert x k n 
   |k/=10      =(convertHelper2 (convertHelper1 x 0 0 k n) 1 0 k n )
   |otherwise  =(convertHelper2 x 1 0 k n)
   
--Zadacha 2

--Proverka za prosto chislo
isPrime::Int->Bool
isPrime n 
   |(n <= 1)  =False  
   |otherwise =(isPrime' 2 n)
   
isPrime'::Int->Int->Bool   
isPrime' current n 
   |current == n         =True
   |mod n current == 0   =False 
   |otherwise            =(isPrime' (current + 1) n)

myPrimeReorder::[t]->[t]
myPrimeReorder (x:xs) = (myPrimeReorderHelper (x:xs) [] [] 2)

--Slepvane na dva lista ys /s prost index/ i zs 
myPrimeReorderHelper::[t]->[t]->[t]->Int->[t]
myPrimeReorderHelper  [] ys zs n = ys++zs
myPrimeReorderHelper (x:xs) ys zs index
   |(isPrime index)  = myPrimeReorderHelper xs (ys++[x]) zs (index+1)
   |otherwise    = myPrimeReorderHelper xs   ys   (zs++[x]) (index+1)
