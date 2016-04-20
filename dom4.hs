main=do
 print (minDistance [(1,1,1),(10,1,1),(5,1,1),(2,1,1)])
 print (maximize [(\x->x*x*x),(\x->x+1)] (-2))
 
--Zada4a 1

minDistance'::[(Double,Double,Double)]->[Double]->[Double]
minDistance' [] zs =zs
minDistance' (x:[]) zs =zs
minDistance' (x:y:xs) zs=(minDistance' (x:xs) (zs++[(minDistanceH x y)]))
                       ++( minDistance' (y:xs) (zs++[(minDistanceH x y)]))

minDistanceH::(Double,Double,Double)->(Double,Double,Double)->Double
minDistanceH (x1,y1,z1) (x2,y2,z2) =((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))**(0.5)

minDistance::[(Double,Double,Double)]->Double
minDistance xs =(minimum (minDistance' xs []))

--Zada4a 2
maximize::[(Double->Double)]->(Double->Double)
maximize ys x 
     |(x<0)     = (-(maximum ( map (\ f -> abs ( f x)) ys))) 
     |otherwise = maximum ( map (\ f -> abs ( f x)) ys)

