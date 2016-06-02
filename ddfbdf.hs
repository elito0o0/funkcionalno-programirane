import Data.List
main=do
 print()
f1::[(Double->Double)]->(Double->Double)
f1 fs =(\x->f1H fs x 0 0)

f1H::[(Double->Double)]->Double->Double->Double->Double
f1H []  _ sum1 count=sum1/count
f1H (f:fs) x sum1 count= f1H fs x (sum1+(f x)) (count+1.0)

f2::[(Double->Double)]->(Double->Double)
f2 fs=(\x->product[f x|f<-fs])
--product (map ($x) fs

data BT=Em|Nd Int BT BT
fG::BT->[Int]
fG Em=[]
fG (Nd x lt rt)=( if(x==(sumC lt)+(sumC  rt)) then [x] else [])++(fG lt)++(fG rt)
 where sumC::BT->Int
       sumC Em=0
       sumC (Nd _ Em Em)=0
       sumC (Nd _ (Nd l _ _)Em)=l
       sumC (Nd _ Em (Nd r _ _))=r
       sumC (Nd _ (Nd l _ _) (Nd r _ _))=r+l

fg::BT->Bool
fg Em =False
fg (Nd _ Em Em)=True
fg (Nd v lt Em)=(findIn v lt)&& (fg lt)
fg (Nd v Em rt)=(findIn v rt)&& (fg rt)
fg (Nd v lt rt)=(findIn v lt)|| (findIn v rt)&&(fg lt)&&(fg rt)
findIn::Int->BT->Bool
findIn _ Em=False
findIn x (Nd v lt rt)=(v==x)||(findIn x lt)||(findIn x rt)

data Result=Pm String String String Int Int|Sm String String Int Int
fC::[Result]->String->String
fC [] _=""
fC ((Pm country n1 n2 _ _):rs) name=if (name==n1||name==n2) then country else (fC rs name)
fC ((Sm _ _ _ _):rs) name =fC rs name

get::[Result]->String->Int
get [] _=0
get ((Pm _ n1 n2 g1 g2):rs) name=(if (name==n1) then g1 else (if(name==n2) then g2 else 0))+(get rs name)
get ((Sm n1 n2 g1 g2):rs) name = (if (name==n1) then g1 else (if (name==n2) then g2 else 0))+(get rs name)

allTeamsFrom::[Result]->String->[String]
allTeamsFrom [] _=[]
allTeamsFrom ((Pm c n1 n2 _ _ ):rs) country=[n1,n2]++(allTeamsFrom rs country)
allTeamsFrom ((Sm _ _ _ _):rs) country=(allTeamsFrom rs country)

sumG::[Result]->String->Int
sumG [] _ =0
sumG ((Pm c n1 n2 _ _ ):rs) name==(if (name==n1) then g1 else (if (name==n2) then g2 else 0))+(sumG rs name)
sumG ((Sm _ _ _ _):rs) name =sumG rs name

f::[Result]->String->String
f rs country= if (d==[]) then "" else snd(maximum d)
   where d=[(sumG team,team)|team<-nub(allTeamsFrom rs country)]
