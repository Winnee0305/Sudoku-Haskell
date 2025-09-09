{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module GameFlow where

import Control.Monad ((>=>))
import GameLogic
import TypesNUtilities
import UserInterface

-- Flow navigation based on input type
processInput :: Input -> IO ()
processInput Quit = displayMessages formatExitMessage -- Quit the program
processInput MainMenu = startProgram -- Navigate to main menu
processInput StartGame = enterName True -- Let user select difficulty before starting the game
processInput Solver = enterName False -- Let user enter name before starting the solver
processInput LoadGame = preLoadGameFlow -- Load game page
processInput InvalidRow = displayMessages formatInvalidRow -- Display invalid row message
processInput InvalidCol = displayMessages formatInvalidCol -- Display invalid column message
processInput InvalidValue = displayMessages formatInvalidValue -- Display invalid value message
processInput InvalidMove = displayMessages formatInvalidMove -- Display invalid move message
processInput _ = displayMessages formatInvalidInput

processMenuAndQuiInput :: Input -> IO ()
processMenuAndQuiInput = processInput

-- Retry helper function
retry :: (a -> IO ()) -> a -> [String] -> IO ()
retry action param invalidMsg = displayMessages invalidMsg >> action param

-- Main program flow
startProgram :: IO () -- First function to be called in the program
startProgram =
  displayMessages (formatMainMenu <> formatInputInstruction) -- Display main menu and input instruction
    >> getUserInput parseMainMenuInput -- Get and parse user input
    >>= processStartProgramInput -- Process the input

processStartProgramInput :: Input -> IO () -- Process the input from startProgram
processStartProgramInput userInput =
  case userInput of
    Invalid -> processInput Invalid >> startProgram -- Invalid input, recursively call startProgram
    _ -> processInput userInput -- Process the input to go to the next step

-- Back to main menu flow
backToMainMenuFlow :: IO (GameState Board) -> (IO (GameState Board) -> IO ()) -> IO ()
backToMainMenuFlow gameStateIO action =
  displayMessages formatBackToMainMenu -- Display the back to main menu message
    >> getUserInput parseYNInput -- Get and parse user input
    >>= \result ->
      case result of
        Yes -> saveFlow gameStateIO (processInput MainMenu) -- If user confirm, go back to main menu
        No -> action gameStateIO -- If user reject, continue the game
        _ -> processInput Invalid >> backToMainMenuFlow gameStateIO action -- If invalid input, recursively call backToMainMenu

-- Quit flow
quitFlow :: IO (GameState Board) -> (IO (GameState Board) -> IO ()) -> IO ()
quitFlow gameStateIO action =
  displayMessages formatQuitGame -- Display the quit game message
    >> getUserInput parseYNInput -- Get and parse user input
    >>= \result ->
      case result of
        Yes -> saveFlow gameStateIO (processInput Quit) -- If user confirm, quit the program
        No -> continueGame gameStateIO -- If user reject, continue the game
        _ -> processInput Invalid >> quitFlow gameStateIO action -- If invalid input, recursively call quitFlow

-- Save flow
saveFlow :: IO (GameState Board) -> IO () -> IO ()
saveFlow gameStateIO action =
  displayMessages formatSaveGame
    >> getUserInput parseYNInput
    >>= \result ->
      case result of
        Yes -> saveAction gameStateIO >> displayMessages formatSavedMessage >> action -- Save the game and display the saved message, then proceed to action
        No -> displayMessages formatNotSavedMessage >> action -- Display the not saved message and proceed to action
        _ -> processInput Invalid >> saveFlow gameStateIO action -- If invalid input, recursively call saveFlow

-- Enter Name flow
enterName :: Bool -> IO () -- After selecting start game, let user enter name
enterName startGameStatus =
  displayMessages formatEnterName
    >> getUserInput parseNameInput -- Get and parse user input
    >>= processNameInput startGameStatus -- Process the input

processNameInput :: Bool -> Input -> IO ()
processNameInput startGameStatus userInput =
  case userInput of
    Name name -> nameConfirmation name startGameStatus -- After entering name, let user confirm the name
    InvalidName -> retry enterName startGameStatus formatInvalidName -- Invalid name, recursively call enterName
    _ -> processMenuAndQuiInput userInput -- Process the input

nameConfirmation :: String -> Bool -> IO () -- After entering name, let user confirm the name
nameConfirmation name startGameStatus =
  displayMessages (formatNameConfirmation name)
    >> getUserInput parseYNInput -- Get and parse user input
    >>= \result -> processConfirmationInput name result startGameStatus -- Process the input

processConfirmationInput :: String -> Input -> Bool -> IO ()
processConfirmationInput name userInput startGameStatus =
  case userInput of
    Yes -> proceedWithGame name startGameStatus -- If user confirm the name, proceed with the game
    No -> enterName startGameStatus -- If user reject the name, let user enter the name again
    _ -> processInput Invalid >> nameConfirmation name startGameStatus -- Invalid input, recursively call nameConfirmation

proceedWithGame :: String -> Bool -> IO ()
proceedWithGame name startGameStatus =
  if startGameStatus
    then
      selectDifficulty (sudokuGameState name) -- If startGameStatus is True, go to selectDifficulty flow
    else
      solverFlow (updateCellStatus (solverGameState name)) -- If startGameStatus is False, go to solverFlow

-- Select Difficulty flow
selectDifficulty :: IO (GameState Board) -> IO ()
selectDifficulty gameStateIO =
  displayMessages (formatDifficultyMenu <> formatInputInstruction) -- Display the difficulty menu and input instruction
    >> getUserInput parseDifficultyInput -- Get and parse user input
    >>= processDifficultyInput gameStateIO -- Process the input

processDifficultyInput :: IO (GameState Board) -> Input -> IO ()
processDifficultyInput gameStateIO userInput =
  case userInput of
    EasyLevel -> startWithDifficulty Easy gameStateIO -- Start the game with the updated state
    NormalLevel -> startWithDifficulty Normal gameStateIO -- Start the game with the updated state
    HardLevel -> startWithDifficulty Hard gameStateIO -- Start the game with the updated state
    Invalid -> retry selectDifficulty gameStateIO formatInvalidInput
    _ -> processMenuAndQuiInput userInput

startWithDifficulty :: Difficulty -> IO (GameState Board) -> IO ()
startWithDifficulty diff gameStateIO =
  startGame (updateGameStateDiff gameStateIO diff) -- Start the game with the updated state (by updating the difficulty)

-- Main Game flow

startGame :: IO (GameState Board) -> IO ()
startGame gameStateIO =
  gameStateIO
    >>= \gameState ->
      generateSudoku (difficulty gameState) -- Generate sudoku board based on difficulty
        >>= \generatedBoard ->
          updateGameState generatedBoard gameStateIO -- Function to update game state with generated board
            >>= \updatedGameState ->
              updateCellStatus (return updatedGameState) -- Function to update the coordinates
                >>= ( updateGameTime
                        >=> ( continueGame . return -- Update the game time and continue the game
                            )
                    )

continueGame :: IO (GameState Board) -> IO () -- Continue the game
continueGame gameStateIO =
  gameStateIO -- Display the menu and instructions
    >>= \gameState ->
      showGame gameState
        >> getUserInput parseGameMenuInput -- Get and parse user input
        >>= \userInput -> processGamePlayInput userInput gameStateIO continueGame -- Process the input

-- Solver flow

solverFlow :: IO (GameState Board) -> IO ()
solverFlow gameStateIO =
  gameStateIO
    >>= \gameState ->
      showSolver gameState -- Display the menu and instructions
        >> getUserInput parseSolverMenuInput
        >>= \result ->
          case result of
            Solve -> solveFlow gameStateIO -- Solve the puzzle
            _ -> processGamePlayInput result gameStateIO solverFlow -- Process the input

solveFlow :: IO (GameState Board) -> IO () -- Start solver after user key in the question sudoku
solveFlow gameStateIO =
  gameStateIO
    >>= \gameState ->
      solveAction (getCurrentBoard gameState) -- Solve the puzzle
        >>= \solvedBoard ->
          if isFilled solvedBoard -- Check is the puzzle solvable
            then
              processIsSolved gameStateIO solvedBoard -- If solvable, process the solved board
            else
              displayMessages formatSolvedFailed >> solverFlow gameStateIO -- If not solvable, display the failed message and recursively call solverFlow

processIsSolved :: IO (GameState Board) -> Board -> IO ()
processIsSolved gameStateIO solvedBoard =
  updateGameState solvedBoard gameStateIO -- Update the game state with the solved board
    >>= ( updateSolved
            >=> ( solvedSuceessFlow . return
                )
        )

solvedSuceessFlow :: IO (GameState Board) -> IO ()
solvedSuceessFlow gameStateIO =
  gameStateIO >>= \gameState ->
    showSolvedInterface gameState -- Display the solved interface
      >> getUserInput parseGameInput -- Get and parse user input
      >>= \result ->
        case result of
          Quit -> quitFlow gameStateIO solvedSuceessFlow -- Quit the program
          MainMenu -> backToMainMenuFlow gameStateIO solvedSuceessFlow -- Navigate to main menu
          _ -> retry solvedSuceessFlow gameStateIO formatInvalidInput

-- Load game flow
preLoadGameFlow :: IO () -- Before load the game, load the records
preLoadGameFlow = loadRecords >>= loadGameFlow . initGameRecords -- Load the game records and pass to loadGameFlow

loadGameFlow :: GameRecords (GameState Board) -> IO () -- Load game flow
loadGameFlow gameRecs =
  showLoadGame (filteredRecords gameRecs) -- Display the load game menu with records
    >> input
    >>= \userInput ->
      let processedInput = parseLoadGameInput userInput (length (filteredRecords gameRecs))
       in processLoadGameInput processedInput gameRecs -- Get and parse user input, then process the input

processLoadGameInput :: Input -> GameRecords (GameState Board) -> IO ()
processLoadGameInput userInput gameRecs =
  case userInput of
    LoadGameNum num -> processLoadGame num (filteredRecords gameRecs) -- Load the game based on the number
    MainMenu -> startProgram -- Navigate to main menu
    Quit -> processInput Quit -- Quit the program
    ShowAll -> loadGameFlow (initGameRecords (fullRecords gameRecs)) -- Show all records
    FilterEasy -> loadGameFlow (filterAction FilterEasy gameRecs) -- Filter the records based on easy difficulty then recursively call loadGameFlow
    FilterNormal -> loadGameFlow (filterAction FilterNormal gameRecs) -- Filter the records based on normal difficulty
    FilterHard -> loadGameFlow (filterAction FilterHard gameRecs) -- Filter the records based on hard difficulty
    FilterSolver -> loadGameFlow (filterAction FilterSolver gameRecs) -- Filter the records based on solver mode
    FilterGame -> loadGameFlow (filterAction FilterGame gameRecs) -- Filter the records based on game mode
    FilterSolved -> loadGameFlow (filterAction FilterSolved gameRecs) -- Filter the records based on solved status
    FilterUnsolved -> loadGameFlow (filterAction FilterUnsolved gameRecs) -- Filter the records based on unsolved status
    SortStatus -> loadGameFlow (sortAction SortStatus gameRecs) -- Sort the records based on status
    SortGenerated -> loadGameFlow (sortAction SortGenerated gameRecs) -- Sort the records based on generated time
    SortElapsed -> loadGameFlow (sortAction SortElapsed gameRecs) -- Sort the records based on elapsed time
    SortName -> loadGameFlow (sortAction SortName gameRecs) -- Sort the records based on name
    SortSaved -> loadGameFlow (sortAction SortSaved gameRecs) -- Sort the records based on last saved time
    _ -> retry loadGameFlow gameRecs formatInvalidLoadInput

processLoadGame :: Int -> [GameState Board] -> IO () -- Process to load game based on the input number
processLoadGame inputNum records =
  updateGameTime (records !! (inputNum - 1)) -- Update the game time on the selected record
    >>= \gameState ->
      updateCurrentCoorFromHistory (return gameState)
        >>= \updatedGameState ->
          -- Update the current coordinates from the history
          case difficulty updatedGameState of -- Match based on difficulty to decide which flow to go
            SolverMode -> processAfterLoad solverFlow (return updatedGameState) -- If solver mode, go to solver flow
            _ -> processAfterLoad continueGame (return updatedGameState) -- If other option (easy, normal, hard), go to continue game flow

processAfterLoad :: (IO (GameState Board) -> IO ()) -> IO (GameState Board) -> IO () -- Process after loading the game
processAfterLoad action gameStateIO =
  gameStateIO >>= \gameState ->
    if solvedToBool (solved gameState) -- Check if the game is solved
      then
        solvedSuceessFlow gameStateIO -- If solved, go to solved success flow
      else
        action (return gameState) -- If not solved, go to the respectively action flow

-- Game Play Input Handling
processGamePlayInput :: Input -> IO (GameState Board) -> (IO (GameState Board) -> IO ()) -> IO ()
processGamePlayInput userInput gameStateIO action =
  gameStateIO
    >>= \gameState ->
      case userInput of
        Fill num -> fillFlow (modifyFillInput (currentCoor gameState) userInput) gameStateIO action -- Fill the number (modify the input into QuickFill) to the board
        QuickFill num row col -> fillFlow userInput gameStateIO action -- Fill the number to the board
        Undo -> undoFlow gameStateIO action -- Undo the last action
        Erase -> eraseFlow (modifyEraseInput (currentCoor gameState)) gameStateIO action -- Erase (modify the input into QuickErase) the number from the board
        QuickErase row col -> eraseFlow userInput gameStateIO action -- Erase the number from the board
        MoveUp -> processMovement userInput gameStateIO action -- Move the current location up
        MoveDown -> processMovement userInput gameStateIO action -- Move the current location down
        MoveLeft -> processMovement userInput gameStateIO action -- Move the current location left
        MoveRight -> processMovement userInput gameStateIO action -- Move the current location right
        MainMenu -> backToMainMenuFlow gameStateIO action -- Navigate to main menu
        Quit -> quitFlow gameStateIO action -- Quit the program
        _ -> retry action gameStateIO formatInvalidInput

fillFlow :: Input -> IO (GameState Board) -> (IO (GameState Board) -> IO ()) -> IO ()
fillFlow userInput gameStateIO action =
  fillAction userInput gameStateIO Answer
    >>= \filledGameState ->
      if isFilled (getCurrentBoard filledGameState)
        then solvedSuceessFlow (updateSolved filledGameState) -- If solved, proceed to solved success flow
        else action (return filledGameState) -- If not solved, display message and continue with the provided action

eraseFlow :: Input -> IO (GameState Board) -> (IO (GameState Board) -> IO ()) -> IO ()
eraseFlow userInput gameStateIO action =
  eraseAction userInput gameStateIO -- Erase the number from the board
    >>= \updatedGameState ->
      updateCellStatus (return updatedGameState) -- Update the cell status
        >>= \updatedGameState2 ->
          action (return updatedGameState2) -- Continue the game

undoFlow :: IO (GameState Board) -> (IO (GameState Board) -> IO ()) -> IO ()
undoFlow gameStateIO action =
  gameStateIO
    >>= ( undoAction
            >=> ( \updatedGameState ->
                    case updatedGameState of
                      Just updatedGameState -> action (return updatedGameState)
                      Nothing -> displayMessages formatUndoFailed >> action gameStateIO
                )
        )

processMovement :: Input -> IO (GameState Board) -> (IO (GameState Board) -> IO ()) -> IO ()
processMovement userInput gameStateIO action =
  gameStateIO
    >>= \gameState ->
      if validateMove userInput (currentCoor gameState) -- Validate the move
        then
          moveAction userInput gameStateIO -- Move the current location
            >>= \updatedGameState ->
              action (updateCellStatus (return updatedGameState)) -- Update the cell status and continue the game
        else
          processInput InvalidMove >> action gameStateIO -- If invalid move, display the invalid move message and recursively call the action
