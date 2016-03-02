module Main where
	--import Prelude hiding (null, lookup, map, filter)
	--import Data.HashMap.Lazy as HashMap		
	import Data.Map
	import qualified Data.Set as Set
	--import Data.Set
	import Data.Maybe
	import Data.Graph.AStar	
	import Debug.Trace
	import Data.List

	--goal = 4
	--jugsList = [3,5]
	goal = 35
	jugsList = [15, 28, 42, 44]
	startVertex = (1,1) : (Prelude.map (\x -> (x, 0)) jugsList)
	goalFunc jugs = (snd (last jugs)) == goal
	--distanceToGoal jugs = abs ((snd (last jugs)) - goal)
	distanceToGoal jugs = minimum ( Prelude.map (\jug -> abs ( snd jug ) - goal) jugs)
	--distanceToGoal jugs = 0
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

	--graph jugs = trace ("\n vertex ya on: " ++ (show jugs) ++ "\n neighbors " ++ (show neighbors) ++ "\n") neighbors
	--	where
	--		pourings = [ ( (pour (fst jugTo) (fst jugFrom) jugs) ) ++ [( (fromJust (elemIndex jugFrom jugsList) ), (fromJust (elemIndex jugTo jugsList)) )] 
	--			| jugTo <- jugs, jugFrom <- jugs,
	--			jugFrom /= jugTo, (snd jugFrom) > 0, (snd jugTo) /= (fst jugTo)]
	--		fillings = [ (fill (fst jugTo) jugs) ++ [( -1 , (fromJust (elemIndex jugTo jugsList)) )] | jugTo <- jugs, (fst jugTo) /= (snd jugTo)]
	--		emptyings = [ (emptyDatJug (fst jugTo) jugs) | jugTo <- jugs, (snd jugTo) > 0]
	--		neighbors = Set.fromList (pourings ++ fillings ++ emptyings)

	graph vertex = trace ("\n vertex ya on: " ++ (show jugs) ++ "\n neighbors " ++ (show neighbors) ++ "\n") neighbors
		where
			pourings = [ [( (fromJust (elemIndex (fst jugFrom) jugsList) ), (fromJust (elemIndex (fst jugTo) jugsList)) )] ++ ( (pour (fst jugTo) (fst jugFrom) jugs) )
				| jugTo <- jugs,
				jugFrom <- jugs,
				jugFrom /= jugTo,
				(snd jugFrom) > 0,
				(snd jugTo) /= (fst jugTo)]
			fillings = [ [( -1 , fromJust $ elemIndex jugToSize jugsList )] ++ (fill jugToSize jugs)
				| jugTo <- jugs,
				let jugToSize = fst jugTo,
				jugToSize /= (snd jugTo)]
			emptyings = [ [( fromJust $ elemIndex (fst jugTo) jugsList , -1 )] ++ (emptyDatJug (fst jugTo) jugs)
				| jugTo <- jugs,
				(snd jugTo) > 0]
			neighbors = Set.fromList (pourings ++ fillings ++ emptyings)
			jugs = tail vertex

	--graphTest = ( graph [(15,2), (28, 1), (42, 15), (44, 44)] )
	graphTest = ( graph startVertex )
	pourTest = pour 15 28 [(15,2), (28, 1), (42, 15), (44, 44)]

	--main = putStrLn "Hello, World!"
	--main = do
	--	print $ aStar graph distanceNeighbors distance goal 1 	