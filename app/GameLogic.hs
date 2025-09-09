{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GameLogic where

import Control.Monad (unless, (>=>))
import Data.Char (isDigit, toLower)
import Data.Either (fromRight)
import Data.List (findIndex, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime, getCurrentTimeZone)
import System.Directory (doesFileExist, removeFile)
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)
import TypesNUtilities
import UserInterface

-- Global Constants

fileName :: String
fileName = "GameRecords.csv" -- Fixed file name for the game records

-- Input Handling
getUserInput :: (String -> Input) -> IO Input
getUserInput parser = input >>= \userInput -> return $ parser userInput -- Prompt and parse the user input by the given parser

parseInput :: [(String, Input)] -> String -> Input
parseInput mapping ipt = fromMaybe Invalid $ lookup (map toLower ipt) mapping -- Parse the input based on the given input mapping

parseMainMenuInput :: String -> Input -- Parse the main menu input
parseMainMenuInput =
  parseInput
    [ ("q", Quit),
      ("g", StartGame),
      ("l", LoadGame),
      ("s", Solver)
    ]

parseSolverMenuInput :: String -> Input -- Parse the solver menu input
parseSolverMenuInput ipt = case map toLower ipt of
  "q" -> Quit
  "m" -> MainMenu
  "u" -> Undo
  "w" -> MoveUp
  "a" -> MoveLeft
  "s" -> MoveDown
  "d" -> MoveRight
  "e" -> Erase
  "p" -> Solve
  _ -> parseCommand ipt

parseNameInput :: String -> Input -- Parse the name input
parseNameInput ipt
  | map toLower ipt `elem` ["q", "m"] = parseInput [("q", Quit), ("m", MainMenu)] ipt
  | length ipt <= 10 && length ipt > 1 = Name ipt -- Length of input should be between 2 and 10
  | otherwise = InvalidName

parseYNInput :: String -> Input -- Parse the yes or no input
parseYNInput =
  parseInput
    [ ("y", Yes),
      ("n", No)
    ]

parseDifficultyInput :: String -> Input -- Parse the difficulty input
parseDifficultyInput =
  parseInput
    [ ("q", Quit),
      ("m", MainMenu),
      ("e", EasyLevel),
      ("n", NormalLevel),
      ("h", HardLevel)
    ]

parseGameInput :: String -> Input -- Parse the game input
parseGameInput =
  parseInput
    [ ("q", Quit),
      ("m", MainMenu)
    ]

parseLoadGameInput :: String -> Int -> Input -- Parse the load game input
parseLoadGameInput ipt recordNum =
  case map toLower ipt of
    "q" -> Quit
    "m" -> MainMenu
    "a" -> ShowAll
    "fe" -> FilterEasy
    "fn" -> FilterNormal
    "fh" -> FilterHard
    "fs" -> FilterSolver
    "fg" -> FilterGame
    "fsol" -> FilterSolved
    "funsol" -> FilterUnsolved
    "ss" -> SortStatus
    "sgt" -> SortGenerated
    "set" -> SortElapsed
    "sn" -> SortName
    "slst" -> SortSaved
    _ -> processParseNotFilterSort ipt recordNum

processParseNotFilterSort :: String -> Int -> Input -- Process the input if it is not filter or sort
processParseNotFilterSort ipt recordNum =
  if all isDigit ipt && not (null ipt)
    then
      let num = read ipt
       in if num > 0 && num <= recordNum
            then LoadGameNum num
            else Invalid
    else Invalid

parseGameMenuInput :: [Char] -> Input -- Parse the game menu input
parseGameMenuInput ipt = case map toLower ipt of
  "q" -> Quit
  "m" -> MainMenu
  "u" -> Undo
  "w" -> MoveUp
  "a" -> MoveLeft
  "s" -> MoveDown
  "d" -> MoveRight
  "e" -> Erase
  _ -> parseCommand ipt

parseCommand :: String -> Input -- Parse the command input
parseCommand ipt =
  case words ipt of
    [str, row, col, value] | str == "f" -> validateAndParseAction str (Just row) (Just col) (Just value)
    [str, row, col] | str == "e" -> validateAndParseAction str (Just row) (Just col) Nothing
    [value] | validSudokuDigit value -> Fill (SudokuDigit (read value))
    _ -> Invalid

validateAndParseAction :: String -> Maybe String -> Maybe String -> Maybe String -> Input -- Validate and parse the action
validateAndParseAction "f" (Just row) (Just col) (Just value) -- For validate quick fill
  | not (validRowColDigit row) = InvalidRow
  | not (validRowColDigit col) = InvalidCol
  | not (validSudokuDigit value) = InvalidValue
  | otherwise = QuickFill (SudokuDigit (read value)) (RowColNum (read row)) (RowColNum (read col))
validateAndParseAction "e" (Just row) (Just col) Nothing -- For validate quick erase
  | not (validRowColDigit row) = InvalidRow
  | not (validRowColDigit col) = InvalidCol
  | otherwise = QuickErase (RowColNum (read row)) (RowColNum (read col))
validateAndParseAction _ _ _ _ = Invalid

validRowColDigit :: String -> Bool
validRowColDigit [c] = c `elem` ['0' .. '8'] -- Ensure it's a single character and in '0'..'8'
validRowColDigit _ = False -- Reject if it's not a single character

validSudokuDigit :: String -> Bool
validSudokuDigit [c] = c `elem` ['1' .. '9'] -- Ensure it's a single character and in '1'..'9'
validSudokuDigit _ = False -- Reject if it's not a single character

modifyFillInput :: Coordinate -> Input -> Input -- Modify the fill input
modifyFillInput (RowColNum row, RowColNum col) (Fill value) = QuickFill value (RowColNum row) (RowColNum col)
modifyFillInput _ userInput = userInput

modifyEraseInput :: Coordinate -> Input -- Modify the erase input
modifyEraseInput (RowColNum row, RowColNum col) = QuickErase (RowColNum row) (RowColNum col)

---- Display Drivers
showGame :: GameState Board -> IO () -- Display the game
showGame gameState = do
  elapsedTimeStr <- getElapsedTimeStr gameState calcGameElapsedTime -- Get the total elapsed time
  generateTimeStr <- formatDateTimeOnCurrentTimeZone (generateTime gameState) -- Format the generated time
  displayMessages (formatGameInterface (getCurrentBoard gameState) (userName gameState) (difficulty gameState) generateTimeStr elapsedTimeStr) -- Display the game interface

showSolver :: GameState Board -> IO () -- Display the solver
showSolver gameState =
  formatDateTimeOnCurrentTimeZone (generateTime gameState) -- Format the generated time
    >>= \generateTimeStr ->
      displayMessages $ -- Display the solver interface
        formatSolverInterface
          (getCurrentBoard gameState)
          (userName gameState)
          generateTimeStr

showSolvedInterface :: GameState Board -> IO () -- Display the solved interface
showSolvedInterface gameState = do
  elapsedTimeStr <- getElapsedTimeStr gameState calcSolvedElapsedTime -- Get the total elapsed time
  generateTimeStr <- formatDateTimeOnCurrentTimeZone (generateTime gameState) -- Format the generated time
  let status =
        -- Get the status to decide which interface to display based on solver / game mode
        case difficulty gameState of -- Check the difficulty of the game
          SolverMode -> False -- If it is solver mode, return False
          _ -> True -- Otherwise, return True
  displayMessages $ formatSolvedInterface status (getCurrentBoard gameState) (userName gameState) (difficulty gameState) generateTimeStr elapsedTimeStr

showLoadGame :: [GameState Board] -> IO () -- Display the load game interface
showLoadGame gameStates =
  displayMessages formatLoadGameInterface1 -- Display the header of the load game interface
    >> case gameStates of
      [] -> displayMessages (formatNoRecord <> formatLoadGameInterface2) -- Display no record message if there is no game record
      _ -> showGameRecords gameStates -- Display the game records

showGameRecords :: [GameState Board] -> IO () -- Display the game records
showGameRecords gameStates =
  getCurrentTimeZone >>= \timeZone ->
    -- Get the current time zone
    displayMessages (formatRecordWithStr "No" "Username" "Difficulty" "Status" "Generated Time" "Elapsed Time" "Last Saved" True) -- Display the header of the game records
      >> displayMessages [verLine]
      >> mapM_ displayMessages (formatRecordWithGameState timeZone gameStates False) -- Display each formatted game record
      >> displayMessages formatLoadGameInterface2 -- Display the footer of the load game interface

-- GameState Handling

updateGameStateDiff :: IO (GameState Board) -> Difficulty -> IO (GameState Board) -- Update the game state with the difficulty
updateGameStateDiff gameStateIO diff =
  gameStateIO >>= \gameState ->
    return $ gameState {difficulty = diff}

updateGameState :: Board -> IO (GameState Board) -> IO (GameState Board) -- Update the game state with the board (using stack)
updateGameState board gameStateIO =
  gameStateIO
    >>= \gameState ->
      return $ gameState {gameHistory = push board (gameHistory gameState)}

updateGameTime :: GameState Board -> IO (GameState Board) -- Update the game state with the current time
updateGameTime gameState =
  getCurrentTime >>= \time ->
    -- Get the current time (IO action)
    return $ gameState {startTime = time}

updateSolved :: GameState Board -> IO (GameState Board) -- Update the game state after solved (specifically field of solved status and elapsed time)
updateSolved gameState =
  calculateTotalElapsedTime (startTime gameState) (elapsedTime gameState) (saveRecStatus gameState) -- Calculate the total elapsed time
    >>= \totalElapsedTime ->
      -- Return the updated game state
      return
        gameState
          { solved = Solved (isFilled (getCurrentBoard gameState)),
            elapsedTime = totalElapsedTime
          }

updateCurrentCoor :: Coordinate -> IO (GameState Board) -> IO (GameState Board) -- Update the game state with the coordinate
updateCurrentCoor coor gameStateIO =
  gameStateIO >>= \gameState ->
    return $ gameState {currentCoor = coor}

updateCurrentCoorFromHistory :: IO (GameState Board) -> IO (GameState Board) -- Update the game state from currentBoard cell status to the current coordinate
updateCurrentCoorFromHistory gameStateIO =
  gameStateIO >>= \gameState ->
    case findCurrentCoor $ getCurrentBoard gameState of
      Just (row, col) -> updateCurrentCoor (RowColNum row, RowColNum col) gameStateIO
      Nothing -> gameStateIO

updateCellStatus :: IO (GameState Board) -> IO (GameState Board) -- Update the game state with the cell status (using current coordinate)
updateCellStatus gameStateIO = gameStateIO >>= \gameState -> updateCellStatusNBoard gameStateIO (getCurrentBoard gameState)

updateCellStatusNBoard :: IO (GameState Board) -> Board -> IO (GameState Board) -- Update the game board with the cell status
updateCellStatusNBoard gameStateIO board =
  gameStateIO >>= \gameState ->
    let (RowColNum row, RowColNum col) = currentCoor gameState
        updatedBoard = board -- Get the current board
        currentCell = board !! row !! col -- Get the current cell (SudokuDigit, CellStatus)
        resetBoard = map (map resetCellStatus) updatedBoard -- Reset the cell status of the board
        setCel = setCellStatusToCurrent currentCell -- Set the specific cell status as the current cell
        updatedBoard2 = replaceAt row (replaceAt col setCel (resetBoard !! row)) resetBoard -- Update the board with the updated cell status
     in return $ gameState {gameHistory = push updatedBoard2 (gameHistory gameState)}

-- GameRecords Handling
updateFilteredRecords :: GameRecords (GameState Board) -> [GameState Board] -> GameRecords (GameState Board) -- Update the filtered records
updateFilteredRecords records filtered = records {filteredRecords = filtered, sortStatus = NotSorted}

updateSortStatus :: GameRecords (GameState Board) -> [GameState Board] -> GameRecords (GameState Board) -- Update the sort status
updateSortStatus records filtered = records {filteredRecords = filtered, sortStatus = inverse (sortStatus records)}

-- Cell Validation (To be used in validatin generate of sudoku board, fill of puzzle and check cell condition)
validateFillWithError :: Board -> Coordinate -> Int -> Either String Bool -- Validate the fill with error message
validateFillWithError board coor val
  | not (validFillRow boardList row val) = Left $ colorize Red "Invalid in row " ++ show row -- Check if the fill is valid in the row
  | not (validFillCol boardList col val) = Left $ colorize Red "Invalid in column " ++ show col -- Check if the fill is valid in the column
  | not (validFillBox board coor val) = Left $ colorize Red "Invalid in box containing " ++ show coor -- Check if the fill is valid in the box
  | otherwise = Right True -- Return True if the fill is valid
  where
    boardList = extractBoardList board
    row = getRow coor
    col = getCol coor

validateFill :: Board -> Coordinate -> Int -> Bool -- Validate the fill
validateFill board coor val =
  fromRight False (validateFillWithError board coor val)

validFillRow :: [[SudokuDigit]] -> Int -> Int -> Bool -- Validate the fill in the row (the value to fill is not exist in the row)
validFillRow boardList row val = notInList (boardList !! row) (SudokuDigit val)

validFillCol :: [[SudokuDigit]] -> Int -> Int -> Bool -- Validate the fill in the column (the value to fill is not exist in the column)
validFillCol boardList col val = notInList (transpose boardList !! col) (SudokuDigit val)

validFillBox :: Board -> Coordinate -> Int -> Bool -- Validate the fill in the box (the value to fill is not exist in the box)
validFillBox board coor val = notInList (extractSpecificBox board coor) (SudokuDigit val) -- Use the extractSpecificBox function to get the list of the box

isQuestionCell :: Board -> Coordinate -> Bool -- Check if the cell is a question cell
isQuestionCell board coor =
  case board !! getRow coor !! getCol coor of -- Get the cell in the board based on the given coordinate
    (SudokuDigit _, Question) -> True -- False indicates it is a question cell
    _ -> False

isFilledCell :: Board -> Coordinate -> Bool -- Check if the cell is a filled cell
isFilledCell board coor =
  case board !! getRow coor !! getCol coor of
    (SudokuDigit 0, _) -> False
    _ -> True

--- Generator (generate a valid sudoku puzzle)
generateSudoku :: Difficulty -> IO Board -- Generate a sudoku puzzle based on the given difficulty
generateSudoku diff =
  generateFullBoard
    >>= \fullBoard -> removeValuesBasedOnDiff fullBoard diff

generateFullBoard :: IO Board
generateFullBoard =
  fillCell (empty Question) 0 -- fill the cells starting from the first cell (using an empty board)
    >>= \generatedBoard ->
      if isFilled generatedBoard -- If the board is filled, return the generated board
        then
          return generatedBoard
        else
          generateFullBoard -- Otherwise, generate the board again

fillCell :: Board -> Int -> IO Board
fillCell board 81 = return board -- Base case: All cells are filled
fillCell board pos =
  -- Recursive case: Fill the cell at the given position
  shuffleM [1 .. 9] -- Shuffle the list of numbers from 1 to 9
    >>= \nums -> tryFillCell board nums pos -- Try to fill the cell with the shuffled numbers

tryFillCell :: Board -> [Int] -> Int -> IO Board
tryFillCell board [] _ = return board -- Base case: No valid number (in nums list) to fill
tryFillCell board (x : xs) pos =
  -- Recursive case: Try with the next number in the shuffled list
  if validateFill board coor x
    then attemptFill board newBoard xs pos -- If the number is valid to fill
    else tryFillCell board xs pos -- If the number is not valid to fill, try with the next number
  where
    coor = posToCoordinate pos -- Convert position to coordinate
    newBoard = updateBoard board coor x Question -- Update the board with the new number

attemptFill :: Board -> Board -> [Int] -> Int -> IO Board -- Attempt to fill the board
attemptFill board newBoard remaining pos =
  fillCell newBoard (pos + 1) -- Move to the next position to fill
    >>= \solvedBoard ->
      if solvedBoard == board -- If the board is not filled
        then
          tryFillCell board remaining pos -- Try to fill the cell again
        else
          return solvedBoard -- Otherwise, return the filled board

updateBoard :: Board -> Coordinate -> Int -> CellStatus -> Board -- Update the board with the given coordinate, value and cell status
updateBoard board coor val cellStatus =
  let row = getRow coor
      col = getCol coor
      updatedRow =
        take col (board !! row)
          <> [(SudokuDigit val, cellStatus)]
          <> drop (col + 1) (board !! row)
   in take row board <> [updatedRow] <> drop (row + 1) board -- Update the board by replacing the row with the updated row

removeValuesBasedOnDiff :: Board -> Difficulty -> IO Board -- Remove values based on the difficulty
removeValuesBasedOnDiff board diff =
  let (minCells, maxCells) = getNumCellsToRemove diff -- Get the range of number of cells to remove based on the difficulty
   in removeCellsByBox board (minCells, maxCells) [0 .. 8] -- Remove cells by box

removeCellsByBox :: Board -> (Int, Int) -> [Int] -> IO Board -- Iterate through each box to remove values
removeCellsByBox board _ [] = return board
removeCellsByBox board (minCells, maxCells) (x : xs) =
  randomRIO (minCells, maxCells) -- Randomly pick from the range of number of cells to remove
    >>= ( removeCellsFromBox board x -- Remove cells from the box
            >=> ( \updatedBoard ->
                    removeCellsByBox updatedBoard (minCells, maxCells) xs -- Recursively call the function again with the remaining boxes
                )
        )

removeCellsFromBox :: Board -> Int -> Int -> IO Board
removeCellsFromBox board boxIndex numCellsToRemove =
  randomPick (boxIndexToBoxCoordinates boxIndex) numCellsToRemove -- Randomly pick cells to remove from the box
    >>= \coorsToRemove ->
      -- Randomly pick coordinates to be removed based on the disired number of cells to remove
      return $ foldl (\updatedBoard coor -> updateBoard updatedBoard coor 0 Answer) board coorsToRemove -- Update the board by removing the cells on the picked coordinates using foldl function

-- Movement Actions
moveAction :: Input -> IO (GameState Board) -> IO (GameState Board) -- Move the current coordinate based on the given input
moveAction userInput gameStateIO =
  gameStateIO >>= \gameState ->
    let coor = currentCoor gameState
        updatedCoor = move userInput coor -- Move the current coordinate based on the given input
     in updateCurrentCoor updatedCoor gameStateIO -- Update the game state with the updated coordinate

move :: Input -> Coordinate -> Coordinate
move userInput (RowColNum row, RowColNum col) =
  case userInput of
    MoveUp -> (RowColNum (row - 1), RowColNum col) -- Up decreases the row
    MoveDown -> (RowColNum (row + 1), RowColNum col) -- Down increases the row
    MoveLeft -> (RowColNum row, RowColNum (col - 1)) -- Left decreases the column
    MoveRight -> (RowColNum row, RowColNum (col + 1)) -- Right increases the column
    _ -> (RowColNum row, RowColNum col)

validateMove :: Input -> Coordinate -> Bool -- Validate the move to ensure the coordinate is within the sudoku board
validateMove userInput (RowColNum row, RowColNum col) =
  case userInput of
    MoveUp -> (row - 1) >= 0
    MoveDown -> (row + 1) < 9
    MoveLeft -> (col - 1) >= 0
    MoveRight -> (col + 1) < 9
    _ -> False

-- Fill Action

fillAction :: Input -> IO (GameState Board) -> CellStatus -> IO (GameState Board) -- Fill the cell based on the given input
fillAction userInput gameStateIO cellStatus =
  gameStateIO
    >>= \gameState ->
      let coor = extractCoordinate userInput -- Extract the coordinate from the input
          val = extractValue userInput -- Extract the value from the input
          board = top (gameHistory gameState) -- Get the current board
       in if not (isQuestionCell board coor) -- Check if the cell is not a question cell
            then fillAfterValidNotQues (return gameState) coor val cellStatus -- Fill the cell after validation if it is not a question cell
            else displayMessages formatFillQuestionErrMsg >> return gameState -- Otherwise, display the error message

fillAfterValidNotQues :: IO (GameState Board) -> Coordinate -> Int -> CellStatus -> IO (GameState Board) -- Fill the cell after validation if it is not a question cell
fillAfterValidNotQues gameStateIO coor val cellStatus =
  gameStateIO
    >>= \gameState ->
      let board = top (gameHistory gameState) -- Get the current board
       in case validateFillWithError board coor val of -- If the fill is valid, Return either Right True or Left error message
            Right _ -> updateCellStatusNBoard gameStateIO (updateBoard board coor val cellStatus) -- If the fill is valid
            Left err -> displayMessages [err] >> gameStateIO -- If the input is invalid

-- Erase action

eraseAction :: Input -> IO (GameState Board) -> IO (GameState Board) -- Erase the cell based on the given input
eraseAction userInput gameStateIO =
  gameStateIO
    >>= \gameState ->
      let coor = extractCoordinate userInput -- Extract the coordinate from the input
          board = top (gameHistory gameState) -- Get the current board
       in if not (isQuestionCell board coor) -- Check if the cell is not a question cell
            then
              eraseAfterValidNotQues gameStateIO coor -- Erase the cell after validation if it is not a question cell
            else
              displayMessages formatEraseQuestionErrMeg >> gameStateIO -- Otherwise, display the error message

eraseAfterValidNotQues :: IO (GameState Board) -> Coordinate -> IO (GameState Board) -- Erase the cell after validation if it is not a question cell
eraseAfterValidNotQues gameStateIO coor =
  gameStateIO
    >>= \gameState ->
      let board = top (gameHistory gameState) -- Get the current board
       in updateCellStatusNBoard gameStateIO (updateBoard board coor 0 Answer) -- Update the game state with the updated board (erased)

-- Undo action
undoAction :: GameState Board -> IO (Maybe (GameState Board))
undoAction gameState
  | difficulty gameState == SolverMode && length (gameHistory gameState) > 2 = updateUndoAction gameState -- If the difficulty is SolverMode and history is more than 2, update the game state after undo
  | length (gameHistory gameState) < 4 = return Nothing -- If history is too short, return Nothing
  | otherwise = updateUndoAction gameState -- Otherwise, update the game state after undo

updateUndoAction :: GameState Board -> IO (Maybe (GameState Board))
updateUndoAction gameState = do
  let poppedBoard = gameState {gameHistory = pop (gameHistory gameState)} -- Pop the board from the history
  updatedGameState <- updateCurrentCoorFromHistory (return poppedBoard) -- Update the game state with the current coordinate
  return (Just updatedGameState)

-- Solve action
solveAction :: Board -> IO Board -- Solve the sudoku puzzle
solveAction board = solveCell board 0

solveCell :: Board -> Int -> IO Board
solveCell board 81 = return board -- Base case: All cells are filled
solveCell board pos
  | isFilledCell board (posToCoordinate pos) = solveCell board (pos + 1) -- If the cell is filled, move to the next cell
  | otherwise = trySolve board [1 .. 9] pos -- Otherwise, try to solve the cell with numbers from 1 to 9

trySolve :: Board -> [Int] -> Int -> IO Board
trySolve board [] pos =
  putStrLn
    ( colorize
        Pink -- Show the process to let the user know the solver is backtracking
        ( "Backtracking at position: "
            ++ show pos
        )
    )
    >> return board
trySolve board (x : xs) pos = do
  let coor = posToCoordinate pos
  putStrLn $ colorize Grey ("Trying number " ++ show x ++ " at position " ++ show pos) -- Show the process to let the user know the solver is trying which number at which position
  if validateFill board coor x -- If the number is valid to fill
    then do
      let updatedBoard = updateBoard board coor x Question -- Update the board with the new number
      solvedBoard <- solveCell updatedBoard (pos + 1) -- Solve the next cell
      if isFilled solvedBoard -- If the board is solved, return the solved board
        then return solvedBoard -- Otherwise, try to solve the cell again
        else trySolve board xs pos -- Try to solve the cell again with the next number
    else trySolve board xs pos -- If the number is not valid to fill, try with the next number

-- Save Record
saveAction :: IO (GameState Board) -> IO () -- Save the game record
saveAction gameStateIO =
  gameStateIO
    >>= saveGameStateToCsv

saveGameStateToCsv :: GameState Board -> IO () -- Save the game state to the CSV file
saveGameStateToCsv gameState = do
  gameStateCSV <- gameStateToCsv gameState
  checkFile -- Check if the file exists
    >> if saveRecStatus gameState -- If the game record is saved previously
      then
        updateRec (generateTime gameState) gameStateCSV -- Update the record
      else
        appendRec gameStateCSV -- Otherwise, append to add the new record

-- CSV Handling

gameStateToCsv :: GameState Board -> IO String -- Convert the game state to CSV format
gameStateToCsv gameState = do
  currentTime <- getCurrentTime
  totalElapsedTime <- getTotalElapsedTimeToSave gameState -- Get the total elapsed time
  return $
    concat
      [ userName gameState,
        "\t",
        show $ difficulty gameState,
        "\t",
        show $ solved gameState,
        "\t",
        show $ getCurrentBoard gameState,
        "\t",
        show $ generateTime gameState,
        "\t",
        show currentTime,
        "\t",
        show totalElapsedTime
      ]

getTotalElapsedTimeToSave :: GameState Board -> IO NominalDiffTime
getTotalElapsedTimeToSave gameState =
  if saveRecStatus gameState && solvedToBool (solved gameState) -- If the game record is saved previously and the game is solved
    then return (elapsedTime gameState) -- Return the elapsed time from the gameState
    else calculateTotalElapsedTime (startTime gameState) (elapsedTime gameState) True -- Otherwise, calculate the total elapsed time again

appendRec :: String -> IO () -- Append the game record to the CSV file
appendRec gameStateCSV = appendFile fileName (gameStateCSV ++ "\n")

findRecIdxInRecs :: String -> [String] -> Int -- Find the record number in the records
findRecIdxInRecs generateTimeStr recordsStr =
  fromMaybe (-1) $ findIndex (\record -> generateTimeStr `elem` splitOn "\t" record) recordsStr -- Find the index of the record based on the unique field (generate time)

updateRec :: UTCTime -> String -> IO () -- Update the game record
updateRec time updateLine =
  loadCSV >>= \recordsStr ->
    updateCSVLines updateLine recordsStr (findRecIdxInRecs (show time) (tail recordsStr)) -- Update the record based on the record index

updateCSVLines :: String -> [String] -> Int -> IO () -- Update the CSV lines
updateCSVLines updateLine records idx =
  let updatedRecords = replaceAt idx updateLine (tail records) -- Replace the record at the given index
   in removeFile fileName -- Remove the file
        >> checkFile -- Check if the file exists
        >> appendFile fileName (unlines updatedRecords) -- Append the updated records to the file

-- Load Records

checkFile :: IO () -- Check if the file exists
checkFile =
  doesFileExist fileName
    >>= \fileExists ->
      unless fileExists initialiseFile -- If the file does not exist, initialise the file

initialiseFile :: IO () -- Initialise the file by writing the header
initialiseFile = writeFile fileName "userName\tdifficulty\tsolved\tboard\tgenerateTime\tlastSaved\telapsedTime\n"

loadCSV :: IO [String]
loadCSV =
  checkFile -- Check if the file exists
    >> fmap lines (readFile fileName) -- Read the file and return the lines

loadRecords :: IO [GameState Board] -- Load the game records
loadRecords =
  loadCSV
    >>= \recordsStr ->
      mapM (csvToGameState (head recordsStr)) (tail recordsStr) -- Convert the CSV records to the list of game states using mapM function

csvToGameState :: String -> String -> IO (GameState Board) -- Convert the CSV to the game state
csvToGameState headerStr csvStr =
  getCurrentTime >>= \currentTime ->
    return
      GameState
        { gameHistory = [board],
          difficulty = diff,
          userName = nameStr,
          saveRecStatus = True,
          generateTime = gt,
          startTime = currentTime,
          lastSavedTime = lst,
          elapsedTime = et,
          solved = solv,
          currentCoor = defaultMove
        }
  where
    header = splitOn "\t" headerStr -- Split the header string
    csv = splitOn "\t" csvStr -- Split the CSV string
    nameStr = extractField csv header "userName" -- Extract the field using header index from the CSV string
    diff = read (extractField csv header "difficulty") :: Difficulty -- Read the difficulty from the CSV string
    board = read (extractField csv header "board") :: Board -- Read the board from the CSV string
    gt = read (extractField csv header "generateTime") :: UTCTime -- Read the generated time from the CSV string
    lst = read (extractField csv header "lastSaved") :: UTCTime -- Read the last saved time from the CSV string
    et = read (extractField csv header "elapsedTime") :: NominalDiffTime -- Read the elapsed time from the CSV string
    solv = read (extractField csv header "solved") :: Solved -- Read the solved status from the CSV string

-- Filter records
filterAction :: Input -> GameRecords (GameState Board) -> GameRecords (GameState Board) -- Filter the records
filterAction userInput records =
  let fullRecs = fullRecords records
      applyFilter =
        case userInput of -- Get the partial function based on the given input
          FilterEasy -> filterByDifficulty Easy
          FilterNormal -> filterByDifficulty Normal
          FilterHard -> filterByDifficulty Hard
          FilterSolver -> filterByDifficulty SolverMode
          FilterGame -> \recs -> mconcat [filterByDifficulty Easy recs, filterByDifficulty Normal recs, filterByDifficulty Hard recs]
          FilterSolved -> filterBySolved (Solved True)
          FilterUnsolved -> filterBySolved (Solved False)
          _ -> id
   in updateFilteredRecords records (applyFilter fullRecs) -- Apply the matched filter to the full records

filterByDifficulty :: Difficulty -> [GameState Board] -> [GameState Board] -- Filter the records by the difficulty
filterByDifficulty diff = filter (\gameState -> difficulty gameState == diff)

filterBySolved :: Solved -> [GameState Board] -> [GameState Board] -- Filter the records by the solved status
filterBySolved solvedParam = filter (\gameState -> solved gameState == solvedParam)

-- Sort records
sortAction :: Input -> GameRecords (GameState Board) -> GameRecords (GameState Board) -- Sort the records
sortAction userInput records =
  let fullRecs = filteredRecords records
      sorting = sortStatusToBool (inverse (sortStatus records)) -- Get the sorting status by inversing the current sorting status (to toggle the sorting status between ascending and descending). True indicates ascending, False indicates descending
      applySort =
        case userInput of -- Get the partial function based on the given input
          SortStatus -> sortByStatus sorting
          SortGenerated -> sortByGenerated sorting
          SortElapsed -> sortByElapsed sorting
          SortName -> sortByName sorting
          SortSaved -> sortBySaved sorting
          _ -> id
   in updateSortStatus records (applySort fullRecs) -- Apply the matched sorter to the full records

sortByStatus :: Bool -> [GameState Board] -> [GameState Board] -- Sort the records by the status
sortByStatus sorting = if sorting then sortOn (show . solved) else sortOn (Down . show . solved)

sortByGenerated :: Bool -> [GameState Board] -> [GameState Board] -- Sort the records by the generated time
sortByGenerated sorting = if sorting then sortOn generateTime else sortOn (Down . generateTime)

sortByElapsed :: Bool -> [GameState Board] -> [GameState Board] -- Sort the records by the elapsed time
sortByElapsed sorting = if sorting then sortOn elapsedTime else sortOn (Down . elapsedTime)

sortByName :: Bool -> [GameState Board] -> [GameState Board] -- Sort the records by the name
sortByName sorting = if sorting then sortOn userName else sortOn (Down . userName)

sortBySaved :: Bool -> [GameState Board] -> [GameState Board] -- Sort the records by the saved time
sortBySaved sorting = if sorting then sortOn lastSavedTime else sortOn (Down . lastSavedTime)