main=do
 print (mymaxdivisor 10 10) -- if d==c
 print (mybc 4 2)
 print (myheron 6 8 10)
 print (mysum 1 6 )
 print (myreverse 4025 0)
 print (isPalindrome 222322)
 print (isPrime 66)
 print (mysumprimes 1 10)
   
--zada4a 0 
mymaxdivisor d c =  if((mod d (c-1))== 0) then (c-1) else (mymaxdivisor d (c-1))
--zada4a 1
fact n= if(n==0 || n==1)then 1 else (n*(fact (n-1)))
mybc n k = (div (fact (n))((fact (k))*(fact(n-k))))
--zada4a 2
myheron a b c =(1/4)*((2*(a^2*b^2+a^2*c^2+b^2*c^2)-(a^4+b^4+c^4))**(1/2))
--zada4a 3
mysum a b =if(a<=b) then (if((mod a 2) ==0) then(a+(mysum (a+1) b)) else (mysum (a+1) b )) else (0)
--zada4a 4
myreverse n m = if(n<10) then (m*10+n) else (myreverse (div n 10) (m*10+(mod n 10)))
--zada4a 5
isPalindrome n = if(n==(myreverse n 0)) then True else False
--zada4a 6
isPrime n = if(n <= 1) then False  else  (isPrime' 2 n) 
isPrime' current n = if(current == n) then True else( if( mod n current == 0) then False else (isPrime' (current + 1) n))
--zada4a 7
mysumprimes a b = if(a<=b) then (if((isPrime a) ==True) then(a+(mysumprimes (a+1) b)) else (mysumprimes (a+1) b )) else (0)
