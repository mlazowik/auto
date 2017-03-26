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

accepts :: (Eq q) => Auto a q -> [a] -> Bool
accepts aut w = any (isAccepting aut) (foldl (\ s c -> List.nub $ concat $ flip (transition aut) c <$> s ) (initStates aut) w)

emptyA :: Auto a ()
emptyA = A {states=[], initStates=[], isAccepting = const False, transition = \ _ _ -> []}

epsA :: Auto a ()
epsA = A {states=[()], initStates = [()], isAccepting = const True, transition = \ _ _ -> []}

symTransition :: (Eq a) => a -> Bool -> a -> [Bool]
symTransition c q a | not q && a == c = [True]
                    | otherwise = []

symA :: (Eq a) => a -> Auto a Bool
symA c = A {states = [True, False], initStates = [False], isAccepting = id, transition = symTransition c}

leftA :: Auto a q -> Auto a (Either q r)
leftA aut = A {
  states = Left <$> states aut
, initStates = Left <$> initStates aut
, isAccepting = either (isAccepting aut) (const False)
, transition = either (\ q c -> Left <$> transition aut q c) (\ _ _ -> [])
}

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA aut1 aut2 = A {
  states = (Left <$> states aut1) ++ (Right <$> states aut2)
, initStates = (Left <$> initStates aut1) ++ (Right <$> initStates aut2)
, isAccepting = either (isAccepting aut1) (isAccepting aut2)
, transition = either (\ q c -> Left <$> transition aut1 q c) (\ q c -> Right <$> transition aut2 q c)
}

thenTransition :: Auto a q1 -> Auto a q2 -> Either q1 q2 -> a -> [Either q1 q2]
thenTransition aut1 aut2 (Left q) c | any (isAccepting aut1) (transition aut1 q c) = (Left <$> transition aut1 q c) ++ (Right <$> initStates aut2)
                                    | otherwise = Left <$> transition aut1 q c
thenTransition _ aut2 (Right q) c = Right <$> transition aut2 q c

hasAcceptingInitState :: Auto a q -> Bool
hasAcceptingInitState aut = any (isAccepting aut) (initStates aut)

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA aut1 aut2 = A{
  states = (Left <$> states aut1) ++ (Right <$> states aut2)
, initStates = (Left <$> initStates aut1) ++ (if hasAcceptingInitState aut1 then Right <$> initStates aut2 else [])
, isAccepting = either (if hasAcceptingInitState aut2 then isAccepting aut1 else const False) (isAccepting aut2)
, transition = thenTransition aut1 aut2
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