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
symA c = A {states=[True, False], initStates = [False], isAccepting = id, transition = symTransition c}

leftA :: Auto a q -> Auto a (Either q r)
leftA aut =