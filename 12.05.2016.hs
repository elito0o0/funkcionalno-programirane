data Lst=Nil|Cons Int Lst
lst::Lst
lst=(Cons 2 (Cons 3 Nil))
cnt::Lst->Int
cnt Nil =0
cnt (Cons _ lst) =1+(cnt lst)

sum1::Lst->Int
sum1 Nil =0
sum1 (Cons x lst)=x+(sum1 lst)


cnv Nil=[]
cnv (Cons x lst)=[x]++(cnv lst)

ins::Int->Int->Lst->Lst

ins 0 x lst=Cons x lst
ins _ x  Nil=Cons x Nil
ins ind x (Cons y lst)=(Cons y (ins (ind-1) x lst))


main=do
 (print (cnv (ins 1 4 lst)))
 (print (perim(Rect 3 4)))
 (print (kld (bt)))
 (print (isBst bt))
data Shape=Square Int|Rect Int Int|Circle Int

perim::Shape->Int
perim (Circle r)=3*r*2
perim (Rect a b)=2*(a+b)
perim (Square a)=4*a

data Bt=Empty| Node Int Bt Bt
bt::Bt
bt=Node 1(Node 2 (Node 10 Empty Empty) Empty) 
                 (Node 3 Empty Empty)
count::Bt->Int
count Empty =0
count (Node _ bt st)=1+(count bt)+(count st)

sm::Bt->Int
sm Empty=0
sm (Node x bt st)=x+(sm bt)+(sm st)
kld::Bt->[Int]
kld Empty=[]
kld (Node x bt st)=[x]++(kld bt)++(kld st)

isBst::Bt->Bool
isBst Empty =True
isBst (Node _ Empty Empty)=True
isBst (Node b (Node a ldt lrd) Empty)=(b>a)&&(isBst ldt)&&(isBst lrd)
isBst (Node b Empty (Node c rlt rrt))=(b<c)&&(isBst rlt)&&(isBst rrt)
isBst (Node b (Node a ldt lrd) (Node c rlt rrt))=(a<b)&&(b<c)&&(isBst ldt)

inss::Int->Bt->Bt
inss x Empty=(Node x Empty Empty)
inss x (Node a lt rt)
  |x<a       =(Node a (inss x lt) rt)
  |x>a       =(Node a lt (inss x rt))
  |otherwise =(Node a lt rt)

