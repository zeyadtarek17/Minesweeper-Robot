type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)





up:: MyState -> MyState
up (S (x,y) z s t)	| (x == 0) = Null
					|otherwise = (S ((x-1),y) z "up" (S (x,y) z s t))

down:: MyState -> MyState
down (S (x,y) z s t)	| (x == 3) = Null
						|otherwise = (S ((x+1),y) z "down" (S (x,y) z s t))

left:: MyState -> MyState
left (S (x,y) z s t)	| (y == 0) = Null
						|otherwise = (S (x,(y-1)) z "left" (S (x,y) z s t))

right:: MyState -> MyState
right (S (x,y) z s t)| (y == 3) = Null
					  |otherwise = (S (x,(y+1)) z "right" (S (x,y) z s t))

collect:: MyState -> MyState
collect (S (x,y) [] "" t) = Null

collect (S (x,y) (h:t) s tail)  | elem (x,y) (h:t) = (S (x,y) (delete (x,y) (h:t)) "collect" (S (x,y) (h:t) s tail))
								| otherwise = Null




delete x [] = []
delete x (h:t) | x==h = delete x t
			   | otherwise = [h] ++ delete x t

nextMyStates::MyState->[MyState]
nextMyStates (S (x,y) z s t) = delete Null [(up (S (x,y) z s t)), (down (S (x,y) z s t)), left (S (x,y) z s t), right (S (x,y) z s t), collect (S (x,y) z s t)]

isGoal::MyState->Bool
isGoal (S (x,y) z s t) = z==[]

search::[MyState]->MyState
search (x:xs) = if isGoal x then x else search (xs ++ (nextMyStates x))

constructSolution :: MyState ->[String]
constructSolution x = reverse (constructSolution_helper x)

constructSolution_helper :: MyState ->[String] 
constructSolution_helper (S x y s Null) = []
constructSolution_helper (S x y s t) = s:(constructSolution_helper t)

solve :: Cell->[Cell]->[String]
solve x y = constructSolution (search [(S x y ("") Null)])



