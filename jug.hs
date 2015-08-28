module Main where
	--import Prelude hiding (null, lookup, map, filter)
	--import Data.HashMap.Lazy as HashMap		
	import Data.Map
	import qualified Data.Set as Set
	--import Data.Set
	import Data.Maybe
	import Data.Graph.AStar	
	import Debug.Trace

	goal = 4
	jugs = [3,5]
	startVertex = Prelude.map (\x -> (x, 0)) jugs
	goalFunc jugs = (snd (last jugs)) == goal
	distanceToGoal jugs = abs ((snd (last jugs)) - goal)
	distanceToNeighbor x y = 1
	main = do		
		print $ aStar graph distanceToNeighbor distanceToGoal goalFunc startVertex

	pour jugTo jugFrom jugs = toList (adjust (return jugToValueNew) jugTo emptyedJugHash)
		where
			jugHash = Data.Map.fromList jugs
			emptyedJugHash = adjust (return jugFromValueNew) jugFrom jugHash 	--empty from jub
			jugFromValueInitial = fromJust (Data.Map.lookup jugFrom jugHash)
			jugToValueInitial = fromJust (Data.Map.lookup jugTo jugHash)
			jugSum = jugFromValueInitial + jugToValueInitial
			jugToValueNew = if jugSum > jugTo then jugTo else jugSum
			fromSum = jugFromValueInitial - (jugTo - jugToValueInitial)
			jugFromValueNew = if fromSum < 0 then 0 else fromSum
	
	fill jugTo jugs = toList (adjust (return jugTo) jugTo jugHash)
		where
			jugHash = Data.Map.fromList jugs

	emptyDatJug jugTo jugs = toList (adjust (return 0) jugTo jugHash)
		where
			jugHash = Data.Map.fromList jugs

	graph jugs = trace ("\n vertex ya on: " ++ (show jugs) ++ "\n neighbors " ++ (show neighbors) ++ "\n") neighbors
		where
			pourings = [ (pour (fst jugTo) (fst jugFrom) jugs) | jugTo <- jugs, jugFrom <- jugs, jugFrom /= jugTo, (snd jugFrom) > 0, (snd jugTo) /= (fst jugTo)]
			fillings = [ (fill (fst jugTo) jugs) | jugTo <- jugs, (fst jugTo) /= (snd jugTo)]
			emptyings = [ (emptyDatJug (fst jugTo) jugs) | jugTo <- jugs, (snd jugTo) > 0]
			neighbors = Set.fromList (pourings ++ fillings ++ emptyings)

	--graphTest = ( graph [(15,2), (28, 1), (42, 15), (44, 44)] )
	graphTest = ( graph startVertex )
	pourTest = pour 15 28 [(15,2), (28, 1), (42, 15), (44, 44)]

	--main = putStrLn "Hello, World!"
	--main = do
	--	print $ aStar graph distanceNeighbors distance goal 1 	