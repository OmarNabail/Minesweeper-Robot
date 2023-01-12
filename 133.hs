-- milestone given type definitions
type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show , Eq)
-- UP
up :: MyState -> MyState
up (S (x,y) g p q) |x < 0 = Null
				   |x > 3 = Null
				   |y < 0 = Null
				   |y > 3 = Null
				   |(x-1) < 0 = Null
				   |otherwise = (S (x-1,y) g "up" (S (x,y) g p q))
-- DOWN
down :: MyState -> MyState
down (S (x,y) g p q) |x < 0 = Null
				     |x > 3 = Null
					 |y < 0 = Null
				     |y > 3 = Null
				     |(x+1) > 3 = Null
				     |otherwise = (S (x+1,y) g "down" (S (x,y) g p q))
-- LEFT
left :: MyState -> MyState
left (S (x,y) g p q) |y < 0 = Null
				     |y > 3 = Null
					 |x < 0 = Null
				     |x > 3 = Null
				     |(y-1) < 0 = Null
				     |otherwise = (S (x,y-1) g "left" (S (x,y) g p q))
-- RIGHT
right :: MyState -> MyState
right (S (x,y) g p q) |y < 0 = Null
					  |x < 0 = Null
				      |x > 3 = Null
					  |y > 3 = Null
					  |(y+1) > 3 = Null
					  |otherwise = (S (x,y+1) g "right" (S (x,y) g p q))
-- COLLECT
collect :: MyState -> MyState
collect (S (x,y) g p q) = if (elem (x,y) g)
						  then S (x,y) (remover (x,y) g) "collect" (S (x,y) g p q)
					      else Null
-- REMOVER
remover x [] = []
remover x (h:t) = if x == h then remover x t else (h : (remover x t))
-- nextMyStates
nextMyStates :: MyState->[MyState]
nextMyStates (S (x,y) g p q) = remover Null ((up (S (x,y) g p q)) : (down (S (x,y) g p q)) : (left (S (x,y) g p q)) : (right (S (x,y) g p q)) : (collect (S (x,y) g p q)) : [])
-- isGoal
isGoal :: MyState->Bool
isGoal Null = False
isGoal (S (x,y) g p q) = if g == [] then True else False
-- search
search :: [MyState]->MyState
search [] = error "out of bounds"
search (h:t) = if isGoal h then h else search ( t ++ (nextMyStates h) ) 
-- constructSolution
constructSolution :: MyState ->[String]
constructSolution Null = [""]
constructSolution (S (x,y) g p q) = if q == Null then [] else  (constructSolution q) ++ [p]
-- solve
solve :: Cell->[Cell]->[String]
solve x y = constructSolution ( search [( S x y "" Null )] )