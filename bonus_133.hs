-- milestone given type definitions
type Cell = (Int,Int)
data MyState = Null | S Int Cell [Cell] String MyState deriving (Show , Eq)
-- UP
up :: MyState -> MyState
up (S a (x,y) g p q) |x < 0 = Null
				     |x > a = Null
				     |y < 0 = Null
				     |y > a = Null
				     |(x-1) < 0 = Null
				     |otherwise = (S a (x-1,y) g "up" (S a (x,y) g p q))
-- DOWN
down :: MyState -> MyState
down (S a (x,y) g p q) |x < 0 = Null
				       |x > a = Null
					   |y < 0 = Null
				       |y > a = Null
				       |(x+1) > a = Null
				       |otherwise = (S a (x+1,y) g "down" (S a (x,y) g p q))
-- LEFT
left :: MyState -> MyState
left (S a (x,y) g p q) |y < 0 = Null
				       |y > a = Null
					   |x < 0 = Null
				       |x > a = Null
				       |(y-1) < 0 = Null
				       |otherwise = (S a (x,y-1) g "left" (S a (x,y) g p q))
-- RIGHT
right :: MyState -> MyState
right (S a (x,y) g p q) |y < 0 = Null
					    |x < 0 = Null
				        |x > a = Null
					    |y > a = Null
					    |(y+1) > a = Null
					    |otherwise = (S a (x,y+1) g "right" (S a (x,y) g p q))
-- COLLECT
collect :: MyState -> MyState
collect (S a (x,y) g p q) = if (elem (x,y) g)
						    then S a (x,y) (remover (x,y) g) "collect" (S a (x,y) g p q)
					        else Null
-- REMOVER
remover x [] = []
remover x (h:t) = if x == h then remover x t else (h : (remover x t))
-- nextMyStates
nextMyStates :: MyState->[MyState]
nextMyStates (S a (x,y) g p q) = remover Null ((up (S a (x,y) g p q)) : (down (S a (x,y) g p q)) : (left (S a (x,y) g p q)) : (right (S a (x,y) g p q)) : (collect (S a (x,y) g p q)) : [])
-- isGoal
isGoal :: MyState->Bool
isGoal Null = False
isGoal (S a (x,y) g p q) = if g == [] then True else False
-- search
search :: [MyState]->MyState
search [] = error "out of bounds"
search (h:t) = if isGoal h then h else search ( t ++ (nextMyStates h) ) 
-- constructSolution
constructSolution :: MyState ->[String]
constructSolution Null = [""]
constructSolution (S a (x,y) g p q) = if q == Null then [] else  (constructSolution q) ++ [p]
-- solve
solve :: Cell->[Cell]->[String]
solve x y = constructSolution ( search [( S (maxer 0 ( x : y )) x y "" Null )] )
--maxer
maxer a [] = a
maxer a ((x,y) : t ) | a > ( max x y ) = maxer a t
					 | x > y = maxer x t
					 | otherwise = maxer y t