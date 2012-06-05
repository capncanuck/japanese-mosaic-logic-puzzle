module Parse (
    Cell,
    Cells,
    Bounds,
    Rows,
    State,
    Table,
    showTable,
    showStates,
    table,
    untable
) where

import Data.Foldable (toList)
import Data.Sequence (Seq (..), fromList, index)

import qualified Data.Sequence as Seq (length)

type State   = Char
type Cell    = (State, Char)
type Cells   = Seq Cell
type Rows    = Seq Cells
type Bounds  = (Int, Int)
type Table   = (Bounds, Rows)

rows :: String -> Rows
rows = fmap (fromList . zip (repeat ' ') . filter (/= '|')) . fromList . lines

unrows :: [String] -> String
unrows [x]      = x
unrows (x : xs) = x ++ "\n" ++ unrows xs

uncolumns :: String -> String
uncolumns []         = []
uncolumns [x]        = '|' : x : "|"
uncolumns (x : xs)   = ('|' : [x]) ++ uncolumns xs

table :: String -> Table
table xs = ((maxX, maxY), rs)
    where
        rs    = rows xs
        maxX = (pred . Seq.length) (rs `index` 0)
        maxY = (pred . Seq.length) rs

hideCrosses :: String -> String
hideCrosses [] = []
hideCrosses (x : xs) | x == 'X'  = ' ' : hideCrosses xs
                       | otherwise = x : hideCrosses xs

untable :: Table -> String
untable = unrows . map (hideCrosses . uncolumns . map fst . toList) . toList . snd

showTable :: Table -> IO ()
showTable = mapM_ putStrLn . lines . unrows . map (uncolumns . map snd . toList) . toList . snd

showStates :: Table -> IO ()
showStates = mapM_ putStrLn . lines . unrows . map (uncolumns . map fst . toList) . toList . snd
