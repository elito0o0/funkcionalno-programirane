main=do
 print (mybc 4 2)
 print (myheron 6 8 10)
 print (mysum 6 1 )
 print (mymaxdivisor 10)
 
 
   -- if d==c 
 
fact n= if(n==0 || n==1)then 1 else (n*(fact (n-1)))
mybc n k = (div (fact (n))((fact (k))*(fact(n-k))))
myheron a b c =(1/4)*((2*(a^2*b^2+a^2*c^2+b^2*c^2)-(a^4+b^4+c^4))**(1/2))
mysum a b =if(a<=b) then (if((mod a 2) ==0) then(a+(mysum (a+1) b)) else (mysum (a+1) b )) else (0)
mymaxdivisor d  =  if((mod d (c-1))== 0) then (c-1) else (mymaxdivisor d (c-1))
print (isPrime 6666)
isPrime n = if(n <= 1) then 0  else  (isPrime' 2 n) 
isPrime' current n = if(current == n) then 1 else( if( mod n current == 0) then 0 else (isPrime' (current + 1) n))
