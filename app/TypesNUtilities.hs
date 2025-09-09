{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module TypesNUtilities where

import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (NominalDiffTime, TimeZone, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import System.Random.Shuffle (shuffleM)
import Text.Printf (printf)

---- Types

-- Class Definitions

class Stack a where
  push :: a -> [a] -> [a] -- Push an element to the stack
  pop :: [a] -> [a] -- Pop an element from the stack
  top :: [a] -> a -- Get the top element of the stack
  emptyStack :: [a] -- Create an empty stack

class Inverse a where
  inverse :: a -> a

class Format a where
  format :: a -> String

class Sudoku a where
  empty :: CellStatus -> a -- Define empty board
  isFilled :: a -> Bool -- Define filled board

class ExtractBoard a where
  extractBoardList :: a -> [[SudokuDigit]] -- Extract the board as a list of SudokuDigits
  extractSpecificBox :: a -> Coordinate -> [SudokuDigit] -- Extract a specific box from the board

class ExtractInput a where
  extractCoordinate :: a -> Coordinate -- Extract the coordinate from the input
  extractValue :: a -> Int -- Extract the value from the input

class Movement a where
  defaultMove :: a -- Define the default coordinate
  moveLeft :: a -> a -- Define the movement to the left
  moveRight :: a -> a -- Define the movement to the right
  moveUp :: a -> a -- Define the movement up
  moveDown :: a -> a -- Define the movement down

---- Data Types

-- SudokuDigit is a newtype of Int with a range of 1 to 9
newtype SudokuDigit = SudokuDigit {getDigit :: Int} deriving (Show, Eq, Read)

instance Format SudokuDigit where
  format :: SudokuDigit -> String
  format (SudokuDigit n) = show n

-- RowColNum is a newtype of Int with a range of 0 to 8
newtype RowColNum = RowColNum Int deriving (Show, Eq)

-- Solved data type
newtype Solved = Solved Bool deriving (Eq)

instance Read Solved where -- Read instance for Solved data type
  readsPrec :: Int -> ReadS Solved
  readsPrec _ userInput =
    case userInput of
      "Solved" -> [(Solved True, "")]
      "Unsolved" -> [(Solved False, "")]
      _ -> [] -- Return an empty list for invalid input

instance Show Solved where -- Show instance for Solved data type
  show :: Solved -> String
  show (Solved True) = "Solved"
  show (Solved False) = "Unsolved"

-- Cell is a tuple of SudokuDigit and CellStatus
type Cell = (SudokuDigit, CellStatus)

instance Format Cell where
  format :: Cell -> String
  format (SudokuDigit 0, Answer) = "   "
  format (SudokuDigit 0, CurrentAnswer) = colorize Red (" " ++ "?" ++ " ")
  format (SudokuDigit 0, Question) = "   "
  format (SudokuDigit 0, CurrentQuestion) = format (SudokuDigit 0, Question)
  format (SudokuDigit n, Answer) = colorize Green (" " ++ show n ++ " ")
  format (SudokuDigit n, CurrentAnswer) = underline (format (SudokuDigit n, Answer))
  format (SudokuDigit n, Question) = " " ++ show n ++ " "
  format (SudokuDigit n, CurrentQuestion) = underline (format (SudokuDigit n, Question))

-- Board data type
type Board = [[Cell]]

instance Stack Board where
  push :: Board -> [Board] -> [Board]
  push a as = a : as -- Adds the new board to the front of the stack

  pop :: [Board] -> [Board]
  pop (_ : as) = as -- Removes the top element of the stack (board)
  pop [] = [] -- Empty stack (base case)

  top :: [Board] -> Board
  top (x : _) = x -- Returns the top element (board)
  top [] = [] -- Empty stack (base case)

  emptyStack :: [Board]
  emptyStack = []

instance Sudoku Board where
  empty :: CellStatus -> Board -- Create an empty board based on the cell status
  empty Question = replicate 9 (replicate 9 (SudokuDigit 0, Question))
  empty Answer = replicate 9 (replicate 9 (SudokuDigit 0, Answer))
  empty CurrentQuestion = replicate 9 (replicate 9 (SudokuDigit 0, CurrentQuestion))
  empty CurrentAnswer = replicate 9 (replicate 9 (SudokuDigit 0, CurrentAnswer))

  isFilled :: Board -> Bool -- Check if the board is filled
  isFilled = not . any (any (isZero . fst)) -- Check if there is any zero in the board
    where
      isZero (SudokuDigit 0) = True
      isZero _ = False

instance ExtractBoard Board where
  extractBoardList :: Board -> [[SudokuDigit]] -- Extract the board as a list of SudokuDigits
  extractBoardList = map (map fst)

  extractSpecificBox :: Board -> Coordinate -> [SudokuDigit] -- Extract as a list of SudokuDigits from a specific box from the board
  extractSpecificBox board (RowColNum row, RowColNum col) =
    concatMap (take 3 . drop boxStartCol) rowsInBox --
    where
      boardList = extractBoardList board
      boxStartRow = (row `div` 3) * 3 -- Calculate the start row of the box (by dividing the row by 3)
      boxStartCol = (col `div` 3) * 3 -- Calculate the start column of the box (by dividing the column by 3)
      rowsInBox = take 3 $ drop boxStartRow boardList -- Take 3 rows starting from the box start row

instance Format Board where
  format :: Board -> String
  format = unlines . formatBoard -- Format the board

-- Coordinate data type
type Coordinate = (RowColNum, RowColNum) -- (row, col)

instance Movement Coordinate where
  defaultMove :: Coordinate -- Define the default coordinate
  defaultMove = (RowColNum 0, RowColNum 0)

  moveLeft :: Coordinate -> Coordinate -- Define the movement to the left
  moveLeft (RowColNum row, RowColNum col) = (RowColNum row, RowColNum (col - 1)) -- Move left by decrementing 1 from the column

  moveRight :: Coordinate -> Coordinate -- Define the movement to the right
  moveRight (RowColNum row, RowColNum col) = (RowColNum row, RowColNum (col + 1)) -- Move right by incrementing 1 to the column

  moveUp :: Coordinate -> Coordinate -- Define the movement up
  moveUp (RowColNum row, RowColNum col) = (RowColNum (row - 1), RowColNum col) -- Move up by decrementing 1 from the row

  moveDown :: Coordinate -> Coordinate -- Define the movement down
  moveDown (RowColNum row, RowColNum col) = (RowColNum (row + 1), RowColNum col) -- Move down by incrementing 1 to the row

-- Color data type
data Color
  = Red
  | Green
  | Blue
  | Cyan
  | Pink
  | Yellow
  | Grey
  | Default
  deriving (Eq, Show)

-- Input data type
data Input -- To be used in pattern matching the input to proceed to respective actions
  = Quit
  | MainMenu
  | StartGame
  | Solver
  | Solve
  | EasyLevel
  | NormalLevel
  | HardLevel
  | Undo
  | Erase
  | Fill SudokuDigit
  | QuickFill SudokuDigit RowColNum RowColNum
  | QuickErase RowColNum RowColNum
  | InvalidRow
  | InvalidCol
  | InvalidValue
  | Invalid
  | Name String
  | InvalidName
  | Yes
  | No
  | Back
  | LoadGame
  | LoadGameNum Int
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | InvalidMove
  | ShowAll
  | FilterEasy
  | FilterNormal
  | FilterHard
  | FilterSolver
  | FilterGame
  | FilterSolved
  | FilterUnsolved
  | SortStatus
  | SortGenerated
  | SortElapsed
  | SortName
  | SortSaved
  deriving (Show, Eq)

instance ExtractInput Input where
  extractCoordinate :: Input -> Coordinate -- Extracting coordinate from input
  extractCoordinate userInput = case userInput of
    QuickFill _ row col -> (row, col)
    QuickErase row col -> (row, col)
    _ -> (RowColNum 0, RowColNum 0)

  extractValue :: Input -> Int -- Extracting value from input
  extractValue userInput = case userInput of
    Fill (SudokuDigit n) -> n
    QuickFill (SudokuDigit n) _ _ -> n
    _ -> 0

-- Difficulty data type
data Difficulty
  = Easy
  | Normal
  | Hard
  | SolverMode
  deriving (Show, Eq, Read)

-- CellStatus data type
data CellStatus
  = Question
  | Answer
  | CurrentQuestion
  | CurrentAnswer
  deriving (Show, Eq, Read)

--  SortStatus data type
data SortStatus
  = Ascending
  | Descending
  | NotSorted
  deriving (Show, Eq)

instance Inverse SortStatus where -- Inverse of SortStatus (To toggle between Ascending and Descending)
  inverse :: SortStatus -> SortStatus
  inverse Ascending = Descending
  inverse Descending = Ascending
  inverse NotSorted = Ascending

-- GameState data type
data GameState a = GameState
  { gameHistory :: [a], -- To store the history of the game (enable undo feature)
    difficulty :: Difficulty,
    userName :: String,
    saveRecStatus :: Bool, -- To store the status of the game record has been saved previously or not
    generateTime :: UTCTime, -- To store the time when the game is generated
    startTime :: UTCTime, -- To store the time when the game is curently started / continued
    lastSavedTime :: UTCTime, -- To store the time when the game is last saved
    elapsedTime :: NominalDiffTime, -- To store the elapsed time of the game
    solved :: Solved, -- To store the status of the game is solved or not
    currentCoor :: Coordinate -- To store the current coordinate of the cursor
  }
  deriving (Show, Eq)

-- GameRecords data type
data GameRecords a = GameRecords
  { fullRecords :: [a], -- To store the full records of the games
    filteredRecords :: [a], -- To store the filtered records of the games
    sortStatus :: SortStatus -- True: Ascending, False: Descending (So that it can be toggled)
  }
  deriving (Show, Eq)

---- Utilities

-- Display Utilities

width :: Int -- Global constant of width for the display
width = 105 :: Int

bold :: String -> String -- Make the text bold
bold text = "\ESC[1m" ++ text ++ "\ESC[0m"

borderVerLine :: String -- Border line for the vertical line
borderVerLine = "+---------|---------|---------+"

eqlLine :: String -- Line of equal signs
eqlLine = colorize Blue (replicate width '=') -- Replicate the equal sign to the width

verLine :: String -- Vertical line
verLine = colorize Blue (replicate width '-')

horLine :: String -- Horizontal line
horLine = "|"

emptyLine :: String -- Empty line
emptyLine = ""

centerAlign :: String -> String -- Center align the text
centerAlign text =
  let len = length text
      padding = max 0 (width - len)
      leftPad = padding `div` 2
      rightPad = padding - leftPad
   in replicate leftPad ' ' ++ text ++ replicate rightPad ' ' -- Add equal padding to the left and right of the text

leftAlign :: String -> String -- Left align the text
leftAlign = printf ("%-" ++ show width ++ "s") -- Using printf to format the text

rightAlign :: String -> String -- Right align the text
rightAlign = printf ("%" ++ show width ++ "s")

colorize :: Color -> String -> String -- Color the text based on the given color
colorize color text = ansiCode color ++ text ++ ansiCode Default -- Using ansi code to color the text

underline :: String -> String -- Underline the text
underline text = "\ESC[4m" ++ text ++ "\ESC[0m"

displayMessages :: [String] -> IO () -- Display the messages
displayMessages textList = mconcat $ putStrLn <$> textList

-- Date Time Handling

formatDateTime :: TimeZone -> UTCTime -> String -- Format the date time based on the given time zone
formatDateTime timeZone time =
  let localTime = utcToLocalTime timeZone time -- Convert UTC to local time
   in formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime -- Format it as a string

formatElapsedTime :: NominalDiffTime -> String -- Format the elapsed time
formatElapsedTime elapsed =
  let totalSeconds = floor elapsed :: Integer -- Convert the elapsed time to seconds
      hours = totalSeconds `div` 3600 -- Calculate the hours
      minutes = (totalSeconds `mod` 3600) `div` 60 -- Calculate the minutes
      seconds = totalSeconds `mod` 60 -- Calculate the seconds
   in if hours > 0
        then show hours ++ "h " ++ show minutes ++ "m " ++ show seconds ++ "s" -- If the hours are greater than 0, show hours, minutes, and seconds
        else show minutes ++ "m " ++ show seconds ++ "s" -- Otherwise, show minutes and seconds

calculateTotalElapsedTime :: UTCTime -> NominalDiffTime -> Bool -> IO NominalDiffTime -- Calculate the total elapsed time
calculateTotalElapsedTime st et status =
  getCurrentTime >>= \currentTime ->
    -- Get the current time
    let diffTime = diffUTCTime currentTime st -- Calculate the difference between the current time and the start time
     in return $
          if status -- If the status is true
            then diffTime + et -- Add the elapsed time to the difference time
            else diffTime -- Otherwise, return the difference time

calcGameElapsedTime :: GameState Board -> IO NominalDiffTime -- Calculate the elapsed time of the game
calcGameElapsedTime gameState = do
  if saveRecStatus gameState -- If the game is saved
    then
      calculateTotalElapsedTime (startTime gameState) (elapsedTime gameState) True -- If the game is saved, calculate the total elapsed time by adding the elapsed time in record
    else
      calculateTotalElapsedTime (startTime gameState) (elapsedTime gameState) False -- If the game is not saved, the current elapsed time is the total elapsed time

calcSolvedElapsedTime :: GameState Board -> IO NominalDiffTime -- Calculate the elapsed time of the solved game
calcSolvedElapsedTime gameState =
  if saveRecStatus gameState && solvedToBool (solved gameState) -- If the game is saved and solved
    then
      return (elapsedTime gameState) -- Return the elapsed time
    else
      calculateTotalElapsedTime (startTime gameState) (elapsedTime gameState) True -- Calculate the elapsed time

formatDateTimeOnCurrentTimeZone :: UTCTime -> IO String -- Format the date time based on the current time zone
formatDateTimeOnCurrentTimeZone time =
  getCurrentTimeZone >>= \timeZone -> return $ formatDateTime timeZone time

getElapsedTimeStr :: GameState Board -> (GameState Board -> IO NominalDiffTime) -> IO String -- Get the elapsed time as a string
getElapsedTimeStr gameState calculation =
  calculation gameState -- Calculate the elapsed time based on the given calculation
    >>= \totalElapsedTime ->
      -- Calculate the total elapsed time
      return $ formatElapsedTime totalElapsedTime -- Format the elapsed time

-- General Utilities
notInList :: (Eq a) => [a] -> a -> Bool -- Check if the element is not in the list
notInList list val = val `notElem` list

transpose :: [[a]] -> [[a]] -- Transpose the list
transpose = foldr (zipWith (:)) (repeat [])

randomPick :: [a] -> Int -> IO [a] -- Randomly pick elements from the list
randomPick list numToPick = shuffleM list <&> take numToPick

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = [] -- Base case: empty list
replaceAt 0 newElem (x : xs) = newElem : xs -- Replace the element at the given index
replaceAt n newElem (x : xs) = x : replaceAt (n - 1) newElem xs -- Recursively process the rest of the list

extractIdx :: (Eq a) => [a] -> a -> Int -- Extract the index of the element in the list
extractIdx list targetEle = fromMaybe 0 (elemIndex targetEle list)

extractField :: [String] -> [String] -> String -> String -- Extract the field from the list
extractField targetList headerlist targetEle = targetList !! extractIdx headerlist targetEle

-- Input Utilities

input :: IO String -- Get the input from the user
input = getLine

-- Utilities of self Defined Data Types

ansiCode :: Color -> String -- Define the ansi code for the color
ansiCode Red = "\ESC[91m"
ansiCode Green = "\ESC[92m"
ansiCode Blue = "\ESC[94m"
ansiCode Default = "\ESC[0m"
ansiCode Cyan = "\ESC[96m"
ansiCode Pink = "\ESC[95m"
ansiCode Yellow = "\ESC[93m"
ansiCode Grey = "\ESC[90m"

getNumCellsToRemove :: Difficulty -> (Int, Int) -- Get the range of number of cells to remove based on the difficulty
getNumCellsToRemove Easy = (1, 3)
getNumCellsToRemove Normal = (4, 5)
getNumCellsToRemove Hard = (6, 8)
getNumCellsToRemove SolverMode = (0, 0)

sortStatusToBool :: SortStatus -> Bool -- Convert SortStatus to Bool
sortStatusToBool Ascending = True
sortStatusToBool Descending = False
sortStatusToBool NotSorted = False

resetCellStatus :: Cell -> Cell -- Reset the cell status (togle between Question and Answer)
resetCellStatus (SudokuDigit val, CurrentQuestion) = (SudokuDigit val, Question)
resetCellStatus (SudokuDigit val, CurrentAnswer) = (SudokuDigit val, Answer)
resetCellStatus cell = cell

setCellStatusToCurrent :: Cell -> Cell -- Set the cell status to current (as  the current location of the cursor)
setCellStatusToCurrent (SudokuDigit val, Question) = (SudokuDigit val, CurrentQuestion)
setCellStatusToCurrent (SudokuDigit val, Answer) = (SudokuDigit val, CurrentAnswer)
setCellStatusToCurrent cell = cell

solvedToBool :: Solved -> Bool -- Convert Solved to Bool
solvedToBool (Solved b) = b

-- Extracting board elements

getCurrentBoard :: GameState Board -> Board -- Get the current board
getCurrentBoard = top . gameHistory

getRow :: Coordinate -> Int -- Get the row from the coordinate
getRow (RowColNum row, _) = row

getCol :: Coordinate -> Int -- Get the column from the coordinate
getCol (_, RowColNum col) = col

posToCoordinate :: Int -> Coordinate -- Convert position to coordinate
posToCoordinate pos = (,) (RowColNum (div pos 9)) (RowColNum (mod pos 9))

boxIndexToBoxCoordinates :: Int -> [Coordinate] -- Convert box index to list of box coordinates
boxIndexToBoxCoordinates boxIndex =
  [ (RowColNum row, RowColNum col)
    | row <- [startRow .. startRow + 2],
      col <- [startCol .. startCol + 2]
  ]
  where
    startRow = boxIndex `div` 3 * 3
    startCol = boxIndex `mod` 3 * 3

findCurrentCoor :: Board -> Maybe (Int, Int)
findCurrentCoor grid =
  listToMaybe
    [ (rowIdx, colIdx)
      | (rowIdx, row) <- zip [0 ..] grid,
        (colIdx, (_, status)) <- zip [0 ..] row,
        status == CurrentQuestion || status == CurrentAnswer
    ]

-- Formatting board

formatBoard :: Board -> [String] -- Format the board
formatBoard board =
  borderVerLine : formatRow board 1 ++ [borderVerLine] -- Combine the border line and the formatted rows

formatRow :: Board -> Int -> [String] -- Format the columns
formatRow [] _ = mempty -- Base case: empty list
formatRow (row : rows) rowNum --
  | rowNum `mod` 3 == 1 && rowNum > 1 -- If the row number is divisible by 3 and greater than 1 (to add the horizontal line)
    =
      bold borderVerLine : formatColumn row : formatRow rows (rowNum + 1) -- Add the bold border line and the formatted column
  | otherwise =
      formatColumn row : formatRow rows (rowNum + 1) -- Otherwise, proceed to the next row

formatColumn :: [Cell] -> String -- Format the row
formatColumn = (++ "|") . concatMap formatSubgrid . chunksOf 3 -- Concatenate the formatted subgrid (for each 3 cells)

formatSubgrid :: [Cell] -> [Char] -- Format the subgrid
formatSubgrid = ("|" ++) . concatMap format -- Concatenate the formatted cell (for each cell)

-- Defining default game states

sudokuGameState :: String -> IO (GameState Board) -- Construct game state with name
sudokuGameState name = do
  currentTime <- getCurrentTime -- Get the current time as UTCTime
  return $
    GameState
      { gameHistory = [empty Question], -- Default game history is empty (with all cells as Question)
        difficulty = Easy, -- Default difficulty is Easy
        userName = name,
        saveRecStatus = False, -- New game, so the record is not saved
        generateTime = currentTime, -- Set the generate time as the current time
        startTime = currentTime, -- Set the start time as the current time
        lastSavedTime = currentTime, -- Set the last saved time as the current time
        elapsedTime = 0, -- Set the elapsed time as 0
        solved = Solved False, -- Set the solved status as False
        currentCoor = defaultMove -- Set the current coordinate as the default coordinate
      }

solverGameState :: String -> IO (GameState Board) -- Construct game state for solver mode with name
solverGameState name = do
  currentTime <- getCurrentTime -- Get the current time as UTCTime
  return $
    GameState
      { gameHistory = [empty Answer], -- Default game history is empty (with all cells as Answer)
        difficulty = SolverMode, -- Default difficulty is SolverMode
        userName = name,
        saveRecStatus = False, -- New game, so the record is not saved
        generateTime = currentTime, -- Set the generate time as the current time
        startTime = currentTime, -- Store the UTCTime here
        lastSavedTime = currentTime, -- Set the last saved time as the current time
        elapsedTime = 0, -- Set the elapsed time as 0
        solved = Solved False, -- Set the solved status as False
        currentCoor = defaultMove -- Set the current coordinate as the default coordinate
      }

-- Defining default game records

initGameRecords :: [GameState Board] -> GameRecords (GameState Board) -- Construct the game records with the full records
initGameRecords records =
  GameRecords
    { fullRecords = records, -- Set the full records as the given records
      filteredRecords = records, -- By default the filtered records is the same as the full records
      sortStatus = NotSorted -- By default the sort status is NotSorted
    }
