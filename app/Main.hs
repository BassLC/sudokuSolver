module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import System.Environment
import System.Exit

-- Value each `Square` in a Sudoku `Grid` can have.  It can only go from 1
-- to 9.
type SquareValue = Int

-- Represents one `Square` in a Sudoku `Grid`.  An Unassigned `Square`
-- has a list of all possible values it can legally have. An Assigned
-- `Square` can only have one value assigned to it.
data Square
  = Unassigned [SquareValue]
  | Assigned SquareValue
  deriving (Show)

-- A Sudoku `Grid` is an array of 81 `Square`s
type Grid = [Square]

type GridIndex = Int

-- A Sudoku `Play` is represented by the value to assign plus where to
-- assign it in the `Grid`
data Play = Play
  { playValue :: SquareValue,
    playIndex :: GridIndex
  }
  deriving (Show)

-- `get(Row|Column|Square)OfIndex` returns a list with all of the
-- (row|column|square)'s indexes that belong to the same
-- (row|column|square) as the given index
getRowOfIndex :: GridIndex -> [GridIndex]
getRowOfIndex index = take 9 [(index `div` 9) * 9 ..]

getColumnOfIndex :: GridIndex -> [GridIndex]
getColumnOfIndex index = take 9 $ iterate (+ 9) (index `mod` 9)

getSquareOfIndex :: GridIndex -> [GridIndex]
getSquareOfIndex index =
  take 9 $
    filter
      ( \idx ->
          let squareOfIndex i = 3 * (i `div` 27) + ((i `mod` 9) `div` 3)
           in squareOfIndex idx == squareOfIndex index
      )
      [0 ..]

replaceNth :: GridIndex -> Square -> Grid -> Grid
replaceNth index square grid = take index grid ++ square : drop (index + 1) grid

-- `solveGrid` tries, from any `Grid`, to return a valid solution. If
-- a `Grid` does not have a solution, it returns `Nothing`. Else, it
-- returns a completed `Grid`.
solveGrid :: Maybe Grid -> Maybe Grid
solveGrid Nothing = Nothing
solveGrid (Just grid)
  | null plays = Just grid
  | isNothing possibleSolution = Nothing
  | otherwise = fromJust possibleSolution
  where
    plays = possibleGridPlays grid
    possibleSolution = find isJust $ map (solveGrid . assignPlayToGrid grid) plays

-- `assignPlayToGrid` checks if a Play is legal for a particular
-- `Grid`. If not, it returns `Nothing`. If it is, it returns a legal
-- `Grid` with that `Play` assigned, and with the propagation of that
-- assignment to all other `Square`s.
assignPlayToGrid :: Grid -> Play -> Maybe Grid
assignPlayToGrid grid play = do
  let assignedGrid = replaceNth (playIndex play) (Assigned (playValue play)) grid
  propagateAssignmentToIndexes assignedGrid (playValue play) (getAffectedIndexes (playIndex play))
  where
    getAffectedIndexes index = nub $ filter (/= index) (getRowOfIndex index ++ getColumnOfIndex index ++ getSquareOfIndex index)
    -- `propagateAssignmentToIndexes` is responsible for propagating
    -- an assigment to all square that are affected by it.  It returns
    -- a legal `Grid` if propagation was successful. In case of a
    -- conflict, it returns `Nothing`.
    propagateAssignmentToIndexes grid _ [] = Just grid -- No more squares to check => everything is good
    propagateAssignmentToIndexes grid assignedValue (indexToCheck : rest) =
      let affectedSquare = grid !! indexToCheck
       in case affectedSquare of
            -- Possible conflicts:
            --
            -- 1. One conflicting square is already assigned with the
            -- value we want to assign.
            --
            -- 2. One conflicting square is unassigned, but it has no
            -- possible values to be assigned to it.
            --
            -- In all other cases, we continue with the propagation.
            Assigned sqPlay | sqPlay == playValue play -> Nothing
            Assigned _ -> propagateAssignmentToIndexes grid assignedValue rest
            Unassigned plays ->
              let remainingPlays = filter (/= assignedValue) plays
               in if null remainingPlays
                    then Nothing
                    else
                      propagateAssignmentToIndexes
                        (replaceNth indexToCheck (Unassigned remainingPlays) grid)
                        assignedValue
                        rest

-- Return a list of all possible legal `Play`s for a `Grid`.
possibleGridPlays :: Grid -> [Play]
possibleGridPlays grid =
  concat $
    -- Sort it so we get the squares with least possibilites first
    sortBy (\a b -> compare (length a) (length b)) $
      zipWith
        ( \square index -> case square of
            Unassigned possibleValues -> map (`Play` index) possibleValues
            _ -> []
        )
        grid
        [0 ..]

-- `importGrid` tries to parse a string into a sudoku `Grid`.
-- Example of a valid string:
-- .8.7......542..3..7......4..2....1...37....928.5........3.21.7..4.....3...9.5..1.
importGrid :: String -> Maybe Grid
importGrid str =
  fst (gridAssignments !! length assignedPlays)
  where
    gridAssignments =
      iterate
        ( \state -> case state of
            (_, []) -> state
            (Nothing, _ : rest) -> (Nothing, rest)
            (Just grid, p : rest) -> (assignPlayToGrid grid p, rest)
        )
        (Just gridFromString, assignedPlays)
    assignedPlays =
      mapMaybe
        ( \(p, i) -> case p of
            Assigned val -> Just $ Play val i
            Unassigned _ -> Nothing
        )
        $ zip gridFromString [0 ..]
    gridFromString =
      map
        ( \chr ->
            if isDigit chr && (chr /= '0')
              then Assigned (digitToInt chr)
              else Unassigned [1 .. 9]
        )
        str

-- `printGrid` pretty-prints a `Grid` onto stdout
printGrid :: Grid -> IO ()
printGrid b = do
  mapM_
    ( \(square, i) -> do
        case square of
          Assigned val -> putStr $ show val
          Unassigned _ -> putStr "."
        when ((i + 1) `mod` 3 == 0 && ((i + 1) `mod` 9 /= 0)) $
          putStr " | "
        when ((i + 1) `mod` 9 == 0) $
          putStr "\n"
        when ((i + 1) `mod` 27 == 0 && ((i + 1) `mod` 81 /= 0)) $
          putStrLn (replicate 16 '-')
    )
    $ zip b [0 ..]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sudokuStr] ->
      case importGrid sudokuStr of
        Just grid -> do
          case solveGrid $ Just grid of
            Nothing -> putStrLn "No possible solution."
            Just sol -> do
              putStrLn "Solution: "
              printGrid sol
        Nothing -> do
          putStrLn "Error: Invalid sudoku string."
          exitFailure
    _ -> do
      putStrLn "Usage: ./sudokuSolver <sudokuString>"
      exitFailure
