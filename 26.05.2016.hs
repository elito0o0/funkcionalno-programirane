import Data.List
main=do
   print (findSuc bt 2) 
   print (expand bt [1,2])
   print (expandAll bt [[1],[2]]) 
   print (allPaths bt)
   print (findP bt 1 6)
data Bt=Em|Nd Bt Bt
data BTree a=Empty|Node a (BTree a) (BTree a)
bt::(BTree Int)
bt=(Node 1 (Node 2 (Node 4 Empty Empty) (Node 6 Empty Empty)) (Node 3 Empty (Node 5 Empty Empty)))
findSuc::(Eq a)=>(BTree a)->a->[a]
findSuc Empty _=[]
findSuc (Node v lt rt) x=if(x==v) then(getV lt)++(getV rt)else (findSuc lt x)++(findSuc rt x)
  where getV Empty=[]
        getV (Node v _ _)=[v]
expand::(Eq a)=>(BTree a)->[a]->[[a]]
expand Empty _ =[[]]
expand bt [] =[[]]
expand bt path=map(\v->path++[v]) (findSuc bt (last path))
expandAll::(Eq a)=>(BTree a)->[[a]]->[[a]]
expandAll Empty _ =[[]]
expandAll bt [[]] =[[]]
expandAll bt ps=concatMap(\path ->expand bt path) ps
allPaths::(Eq a)=>(BTree a)->[[a]]
allPaths Empty=[[]]
allPaths bt@(Node v _ _)=helper bt [[v]] []
 where helper _ [] paths=nub paths 
       helper br curPaths  paths=helper bt (expanded++(map tail expanded)) (paths ++ expanded)
         where expanded =expandAll bt curPaths

findP::(Eq a)=>(BTree a)->a->a->[[a]]
findP bt start end=filter (\path->(head path)==start&&((last path)==end)) (allPaths bt)
data Btp a =Emp|Nod a Int (Btp a) (Btp a) 

btl::(Btp Int)
btl=Nod 1 0 (Nod 2 10 (Nod 4 1 Emp Emp) (Nod 5 2 Emp Emp)) (Nod 3 11 (Nod 6 3 Emp Emp) Emp)
{-findSuc'::(Btp Int)->[(Int,Int)]
expand'::(Btp Int)->[(Int,Int)]->[[(Int,Int)]]
expandAll'::(Btp Int)->[[(Int,Int)]]->[[(Int,Int)]]
allPaths'::(Btp Int)->[[(Int,Int)]]
-}
minP::(Btp Int)->[Int]
minP bt= snd minimum(map (\path->(sum(map snd path)),(map fst path))(allPaths' bt))
