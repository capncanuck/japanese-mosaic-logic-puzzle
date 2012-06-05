module Composition where

-- Inspired by http://stackoverflow.com/questions/5821089/haskell-function-composition-operator-of-type-cd-abc-abd
(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) = (.) . (.)

(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) = (.) . (.) . (.)
