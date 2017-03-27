module Auto
  ( Auto
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

data Auto a q = A
  { states :: [q]
  , initStates :: [q]
  , isAccepting :: q -> Bool
  , transition :: q -> a -> [q]
  }

instance (Show a, Enum a, Bounded a, Show q) =>
         Show (Auto a q) where
  show = show . toLists

transitions
  :: (Functor t, Foldable t, Eq q)
  => Auto a q -> t q -> a -> [q]
transitions aut fromStates c = List.nub $ concat $ flip (transition aut) c <$> fromStates

accepts
  :: (Eq q)
  => Auto a q -> [a] -> Bool
accepts aut w = any (isAccepting aut) (List.foldl' (transitions aut) (initStates aut) w)

trashTransition _ _ = []

emptyA :: Auto a ()
emptyA = A {states = [], initStates = [], isAccepting = const False, transition = trashTransition}

epsA :: Auto a ()
epsA = A {states = [()], initStates = [()], isAccepting = const True, transition = trashTransition}

symA
  :: (Eq a)
  => a -> Auto a Bool
symA c =
  A {states = [True, False], initStates = [False], isAccepting = id, transition = \q a -> [True | not q && a == c]}

leftA :: Auto a q -> Auto a (Either q r)
leftA aut =
  A
  { states = Left <$> states aut
  , initStates = Left <$> initStates aut
  , isAccepting = either (isAccepting aut) (const False)
  , transition = either ((fmap Left .) . transition aut) trashTransition
  }

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA aut1 aut2 =
  A
  { states = (Left <$> states aut1) ++ (Right <$> states aut2)
  , initStates = (Left <$> initStates aut1) ++ (Right <$> initStates aut2)
  , isAccepting = either (isAccepting aut1) (isAccepting aut2)
  , transition = either ((fmap Left .) . transition aut1) ((fmap Right .) . transition aut2)
  }

addLeftToRight :: Auto a q1 -> Auto a q2 -> [q1] -> [Either q1 q2]
addLeftToRight aut1 aut2 leftStates =
  (Left <$> leftStates) ++
  (if any (isAccepting aut1) leftStates
     then Right <$> initStates aut2
     else [])

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA aut1 aut2 =
  A
  { states = (Left <$> states aut1) ++ (Right <$> states aut2)
  , initStates = addLeftToRight aut1 aut2 (initStates aut1)
  , isAccepting = either (const False) (isAccepting aut2)
  , transition = either ((addLeftToRight aut1 aut2 .) . transition aut1) ((fmap Right .) . transition aut2)
  }

fromLists
  :: (Eq q, Eq a)
  => [q] -> [q] -> [q] -> [(q, a, [q])] -> Auto a q
fromLists s i a t =
  A
  { states = s
  , initStates = i
  , isAccepting = (`elem` a)
  , transition =
      \q c ->
        concatMap
          (\(qq, cc, qs) ->
             if q == qq && c == cc
               then qs
               else [])
          t
  }

notTrash :: (q, a, [q]) -> Bool
notTrash (_, _, qs) = not $ null qs

toLists
  :: (Enum a, Bounded a)
  => Auto a q -> ([q], [q], [q], [(q, a, [q])])
toLists aut =
  ( states aut
  , initStates aut
  , filter (isAccepting aut) (states aut)
  , filter notTrash (map (\(q, c) -> (q, c, transition aut q c)) [(q, c) | q <- states aut, c <- [minBound ..]]))