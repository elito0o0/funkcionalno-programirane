{-
[1,2,3] (+) 20
foldr->1+(2+(3+20))
foldl->((20+1)+2)+3)

count::Int->[Int]->Int
count x xs=length[y|y<-ys,x==y]
--count x xs =length(filter(\y->x==y) xs
occ1 xs ys=(map(\x->count x ys) xs)
occ1 xs ys =[count x ys|x<-xs]
occ2 xs ys =[x|x<-xs,count x ys>1]
occ2 xs ys=filter(\x->count x ys>1) xs
image::[Int]->[Int]->Bool
image (x:xs) (y:ys)=imageH xs ys (x-y)
imageH::[Int]->[Int]->Int->Bool
imageH [] _ _ =True
imageH (x:xs) (y:ys) p=if(x-y/=p)then False else imageH xs ys p
matchLengths xss yss=image (map length xss) (map length yss)
zip::[a]->[b]->[(a,b)]
zip [1,2] [1,0]->[(1,1) ,(2,0)]
zipWith::(a->b->c)->[a]->[b]->[c]
zipWith (+) [1,2,0] [3,5,7] ->[4,7,7]
zipWith _ [] _=[]
zipWith _ _ []=[]
zipWith f (x:xs) (y:ys) =(f x y):(zipWith f xs ys)
(zipWith (\a b->if a>b then 1 else -1) [1,2,0] [0,1,2]->[1,1,-1]-}
zero::[[Int]]->[[Int]]
zeroH::[Int]->[Int]
zeroH xs=if(elem 0 xs) then (zero1) else xs
zero1 xs=[0|x<-xs]
--zero1 xs = map(\x->0) xs
--transpose(zero (transpose xs))
--firstRow xss=head xss
--firstCol xss=map head xss
--bez1 xss=map tail xss
transpose::[[t]]->[[t]]
transpose []=[]
transpose ([]:_)=[]
transpose matrix=(firstCol matrix):(transpose (bez1 matrix))
rps::[Int]->[Int]
rps xs =filter(\x->elem x(delete x xs)) xs
--rps [1,2,3,2])->[2,2]
map rps [[1,2],[2,2,1]->[[],[2,2]]->[2,2]
concat::[[t]]->[t]
concat xss=foldl (++) [] xss
maxD xss=maximum(concat (map rps xss))
