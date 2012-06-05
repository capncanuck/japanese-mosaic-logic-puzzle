module Strategy (
    solve
) where

import Data.Accessor.Basic (compose)

import Parse (Table)
import TableUtils

solve :: Table -> Table
solve t | isEmpty t = t
        | otherwise = compose tasks t

tasks :: [Table -> Table]
tasks =
    [zeros,
    nines,
    sides,
    corners] ++ bruteForce ++ bruteForce

zeros :: Table -> Table
zeros = basic cross allCells '0'

nines :: Table -> Table
nines = basic fill allCells '9'

sides :: Table -> Table
sides = basic fill sideCells '6'

corners :: Table -> Table
corners = basic fill cornerCells '4'

basic :: TableUpdate -> CellSet -> Char -> Table -> Table
basic f1 f2 c t@(b, rs) = (b, rs')
    where
        rs' = (f1 rs . concatMap (around b) . find c f2) t

intermediate :: TableUpdate -> TickOff -> Char -> Table -> Table
intermediate f1 f2 c t@(b, rs) = (b, rs')
    where
        rs' = (f1 rs . concatMap (f2 c t) . find c allCells) t

tickTable :: TableUpdate -> TickOff -> Table -> Table
tickTable f1 f2 = compose (map (intermediate f1 f2) "12345678")

fillTable :: Table -> Table
fillTable = tickTable fill fillIn

crossTable :: Table -> Table
crossTable = tickTable cross crossOut

bruteForce :: [Table -> Table]
bruteForce = [crossTable, fillTable]
