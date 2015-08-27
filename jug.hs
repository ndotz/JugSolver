module Main where
	--import Prelude hiding (null, lookup, map, filter)
	--import Data.HashMap.Lazy as HashMap	
	--import qualified Data.Set as Set
	--import Data.Graph.AStar
	--3 and 5 gallon
	--goal 4 gallon
	--graph x where x is hash like this [(3, 2), (5,1)]	

	--graph jugs = [(a, b) | jug1 <- jugs, jug2 <- jugs, jug1 /= jug2, ]
	--graph jugs = foldl (\listOfStates jug1 -> foldl (\jug2 -> ) [] jugs ) [] jugs

	pour jugTo jugFrom = ( (fst jugTo), (snd jugTo) + (snd jugFrom ))
	--adjustTest = adjust (return 15) 3 jugList
	--	where jugList = fromList [(3 :: Int,2), (5, 1)]

	--graph jugs = [  [ (pour jugOuter jugInner) | jugInner <- jugs, jugOuter /= jugInner ] | jugOuter <- jugs ]
	graph jugs = filter (\x -> snd x > fst x) combos
		where
			combos = [ (pour jugTo jugFrom) | jugTo <- jugs, jugFrom <- jugs, jugFrom /= jugTo]
	
	--graph jugs = [a | a <- jugs]

	jugTest = ( graph [(15,2), (28, 1), (42, 15), (44, 44)] )

	--pourTest = do
	--	print $ pour (3,2) (5, 1)

	distance 1 = 2
	distance 2 = 1
	distance 3 = 5
	distance 4 = 0

	distanceNeighbors x y = 1

	goal 4 = True
	goal x = False

	--main = putStrLn "Hello, World!"
	--main = do
	--	print $ aStar graph distanceNeighbors distance goal 1 