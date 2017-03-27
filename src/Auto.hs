module Auto
(
  Auto -- FIXME: remove before returning the task
, accepts
, emptyA
, epsA
, leftA
, symA
, sumA
, thenA
, fromLists
, toLists
) where

import qualified Data.List as List

data Auto a q = A {
  states :: [q]
, initStates :: [q]
, isAccepting :: q -> Bool
, transition :: q -> a -> [q]
}

instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
  show = show . toLists

transitions :: (Functor t, Foldable t, Eq q) => Auto a q -> t q -> a -> [q]
transitions aut fromStates c = List.nub $ concat $ flip (transition aut) c <$> fromStates

accepts :: (Eq q) => Auto a q -> [a] -> Bool
accepts aut w = any (isAccepting aut) (List.foldl' (transitions aut) (initStates aut) w)

trashTransition _ _ = []

emptyA :: Auto a ()
emptyA = A {states=[], initStates=[], isAccepting = const False, transition = trashTransition}

epsA :: Auto a ()
epsA = A {states=[()], initStates = [()], isAccepting = const True, transition = trashTransition}

symTransition :: (Eq a) => a -> Bool -> a -> [Bool]
symTransition c q a | not q && a == c = [True]
                    | otherwise = []

symA :: (Eq a) => a -> Auto a Bool
symA c = A {states = [True, False], initStates = [False], isAccepting = id, transition = symTransition c}

leftTransition :: Auto a q -> q -> a -> [Either q r]
leftTransition aut q c = Left <$> transition aut q c

rightTransition :: Auto a q -> q -> a -> [Either l q]
rightTransition aut q c = Right <$> transition aut q c

leftA :: Auto a q -> Auto a (Either q r)
leftA aut = A {
  states = Left <$> states aut
, initStates = Left <$> initStates aut
, isAccepting = either (isAccepting aut) (const False)
, transition = either (leftTransition aut) trashTransition
}

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA aut1 aut2 = A {
  states = (Left <$> states aut1) ++ (Right <$> states aut2)
, initStates = (Left <$> initStates aut1) ++ (Right <$> initStates aut2)
, isAccepting = either (isAccepting aut1) (isAccepting aut2)
, transition = either (leftTransition aut1) (rightTransition aut2)
}

hasAcceptingInitState :: Auto a q -> Bool
hasAcceptingInitState aut = any (isAccepting aut) (initStates aut)

nextAccepting :: Auto a q -> q -> a -> Bool
nextAccepting aut q c = any (isAccepting aut) (transition aut q c)

leftToRightTransition :: Auto a q1 -> Auto a q2 -> q1 -> a -> [Either q1 q2]
leftToRightTransition aut1 aut2 q c = (Left <$> transition aut1 q c) ++ (if nextAccepting aut1 q c then Right <$> initStates aut2 else [])

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA aut1 aut2 = A{
  states = (Left <$> states aut1) ++ (Right <$> states aut2)
, initStates = (Left <$> initStates aut1) ++ (if hasAcceptingInitState aut1 then Right <$> initStates aut2 else [])
, isAccepting = either (if hasAcceptingInitState aut2 then isAccepting aut1 else const False) (isAccepting aut2)
, transition = either (leftToRightTransition aut1 aut2) (rightTransition aut2)
}

fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists s i a t = A {
  states = s
, initStates = i
, isAccepting = (`elem` a)
, transition = \ q c -> concatMap (\ (qq, cc, x) -> if q == qq && c == cc then x else []) t
}

toLists :: (Enum a, Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists aut = (
  states aut,
  initStates aut,
  filter (isAccepting aut) (states aut),
  filter (\ (_, _, qs) -> not $ null qs) (map (\ (q, c) -> (q, c, transition aut q c)) [ (q, c) | q <- states aut, c <- [minBound..] ]))