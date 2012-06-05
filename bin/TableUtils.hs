module TableUtils (
    CellSet,
    TableUpdate,
    TickOff,
    allCells,
    around,
    cornerCells,
    cross,
    crossOut,
    fill,
    fillIn,
    find,
    isEmpty,
    sideCells
) where

import Data.List (nub)

import Data.Sequence ((<|), dropWhileL, empty, index)
import qualified Data.Sequence as Seq (adjust, null, splitAt)

import Composition ((.*))
import Parse (Bounds, Rows, Cell, Cells, State, Table)
import Tuple (pushFst)

type CellSet     = (Int, Int) -> [(Int, Int)]
type TableUpdate = Rows -> [(Int, Int)] -> Rows
type TickOff     = Char -> Table -> (Int, Int) -> [(Int, Int)]

(@:) :: Rows -> (Int, Int) -> Cell
(@:) rs (x, y) = (rs `index` x) `index` y

(@<) :: Rows -> (Int, Int) -> State
(@<) = fst .* (@:)

(@>) :: Rows -> (Int, Int) -> Char
(@>) = snd .* (@:)

isEmpty :: Table -> Bool
isEmpty = Seq.null . dropWhileL isEmptyRow . snd
    where
        isEmptyRow :: Cells -> Bool
        isEmptyRow = Seq.null . dropWhileL (\ (x, y) -> x == 'X' || y == ' ')

adjust :: Rows -> Char -> (Int, Int) -> Rows
adjust rs c (m, n) | Seq.null rs = empty
                   | m == 0     = Seq.adjust (pushFst c) n x <| y
                   | otherwise  = x <| adjust y c (m - 1, n)
    where
        x      = u `index` 0
        (u, y) = Seq.splitAt 1 rs

adjusts :: Char -> Rows -> [(Int, Int)] -> Rows
adjusts c = foldl (\ rs x -> adjust rs c x)

fill :: TableUpdate
fill = adjusts '#'

cross :: TableUpdate
cross = adjusts 'X'

allAround :: CellSet
allAround (x, y) = [(i, j) | i <- [x - 1 .. x + 1], j <- [y - 1 .. y + 1]]

around :: Bounds -> CellSet
around b = filter (inBounds b) . allAround

inBounds :: Bounds -> (Int, Int) -> Bool
inBounds (x, y) (i, j) | i < 0 || j < 0 = False
                       | i > x || j > y = False
                       | otherwise      = True

fillIn :: TickOff
fillIn c (b, rs) d | length a == n = a
                   | otherwise     = []
    where
        n = read [c]
        a = (filter (\ x -> rs @< x /= 'X') . around b) d

crossOut :: TickOff
crossOut c (b, rs) d | length f == length a - n = f
                     | otherwise                = []
    where
        n = read [c]
        a = around b d
        f = filter (\ x -> rs @< x /= '#') a

find :: Char -> CellSet -> Table -> [(Int, Int)]
find c f (b, rs) = (filterCells rs c . f) b

filterCells :: Rows -> Char -> [(Int, Int)] -> [(Int, Int)]
filterCells rs c = filter (\ x -> rs @> x == c)

allCells :: CellSet
allCells (x, y) = [(i, j) | i <- [0 .. x], j <- [0 .. y]]

cornerCells :: CellSet
cornerCells (x, y) = [(i, j) | i <- [0, x], j <- [0, y]]

sideCells :: CellSet
sideCells (x, y) = nub ([(i, j) | i <- [0 .. x], j <- [0, y]] ++ [(i, j) | i <- [0, x], j <- [0 .. y]])
