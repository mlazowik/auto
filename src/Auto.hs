module Auto where

data Auto a q = A {
  states :: [q]
, initStates :: [q]
, isAccepting :: q -> Bool
, transition :: q -> a -> [q]
}

-- accepts :: (Eq q) => Auto a q -> [a] -> Bool

emptyA :: Auto a ()
emptyA = A {states=[], initStates=[], isAccepting = const False, transition = \ _ _ -> []}

epsA :: Auto a ()
epsA = A {states=[()], initStates = [()], isAccepting = const True, transition = \ _ _ -> []}

symTransition :: (Eq a) => a -> Bool -> a -> [Bool]
symTransition c q a | not q && a == c = [True]
                    | otherwise = []

symA :: (Eq a) => a -> Auto a Bool
symA c = A {states = [True, False], initStates = [False], isAccepting = id, transition = symTransition c}

leftAccepting :: Auto a q -> Either q r -> Bool
leftAccepting aut (Left q) = isAccepting aut q
leftAccepting _ _ = False

leftTransition :: Auto a q ->  Either q r -> a -> [Either q r]
leftTransition aut (Left q) c = Left <$> transition aut q c
leftTransition _ _ _ = []

leftA :: Auto a q -> Auto a (Either q r)
leftA aut = A {
  states = Left <$> states aut
, initStates = Left <$> initStates aut
, isAccepting = leftAccepting aut
, transition = leftTransition aut
}

sumAccepting :: Auto a q1 -> Auto a q2 -> Either q1 q2 -> Bool
sumAccepting aut1 _ (Left q) = isAccepting aut1 q
sumAccepting _ aut2 (Right q) = isAccepting aut2 q

sumTransition :: Auto a q1 -> Auto a q2 -> Either q1 q2 -> a -> [Either q1 q2]
sumTransition aut1 _ (Left q) = Left <$> transition aut1 q
sumTransition _ aut2 (Right q) = Right <$> transition aut2 q

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA aut1 aut2 = A {
  states = (Left <$> states aut1) ++ (Right <$> states aut2)
, initStates = (Left <$> initStates aut1) ++ (Right <$> initStates aut2)
, isAccepting = sumAccepting aut1 aut2
, transition = sumTransition aut1 aut2
}