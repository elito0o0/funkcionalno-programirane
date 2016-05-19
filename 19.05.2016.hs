data Bt=Empty| Node Int Bt Bt
bt::Bt
bt=Node 1(Node 2 (Node 3 (Node 5 Empty Empty) Empty) (Node 4 Empty Empty) )
                 (Node 5 Empty (Node 2 (Node 7 Empty Empty) Empty))
g1::Bt->[Int]
g1 Empty=[]
g1 (Node x Empty Empty)=[x]
g1 (Node x lt rt)=(g1 lt)++(g1 rt)

main=do
  print (g1 bt)
  print (g2 bt (+))
  print (g3 bt)
  print (g4 bt)
  print (g5 bt 2)
  
g2::Bt->((Int->Int->Int)->Int)

g2 bt f=foldl f 0 (g1 bt)

g3::Bt->[(Int,Int)]

g3 bt=g3H bt 0
g3H Empty _=[]
g3H (Node x lt rt) lev =[(x,lev)]++(g3H lt (lev+1))++(g3H rt (lev+1))

g4::Bt->[(Int,Int)]
g4 Empty=[]
g4 (Node x lt rt) =[(x,length(left++right))]++left++right
   where left=g4 lt
         right=g4 rt
g5::Bt->Int->[[Int]]
g5 Empty _ =[[]]
g5 (Node x Empty Empty) _=[[]]
g5 (Node x (Node y lt rt) Empty) s=(if (s==x )then[[x,y]] else [])++(g5 (Node y lt rt) s)
