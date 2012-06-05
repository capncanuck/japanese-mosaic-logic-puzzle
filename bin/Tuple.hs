module Tuple where

pushFst :: c -> (a, b) -> (c, b)
pushFst u (x, y) = (u, y)

pushSnd :: c -> (a, b) -> (a, c)
pushSnd v (x, y) = (x, v)
