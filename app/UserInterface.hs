module UserInterface where

import Data.Char (toUpper)
import Data.Time (TimeZone)
import Text.Printf (printf)
import TypesNUtilities

-- Page Interfaces

formatPageHeader :: [String] -> [String]
formatPageHeader text =
  [ emptyLine,
    eqlLine
  ]
    <> map (colorize Cyan . centerAlign) text
    <> [eqlLine]

formatMainMenu :: [String]
formatMainMenu =
  formatPageHeader ["Welcome to Sudoku!"]
    <> [ colorize Green "[G]" ++ " Start Game",
         colorize Blue "[S]" ++ " Use Sudoku Solver",
         colorize Yellow "[L]" ++ " Load Saved Game",
         colorize Red "[Q]" ++ " Quit Program"
       ]

formatEnterName :: [String]
formatEnterName =
  formatPageHeader ["Enter Your NAME (Max 10 characters)"]
    <> formatOptions
    <> formatInputInstruction

formatNameConfirmation :: String -> [String]
formatNameConfirmation name =
  formatPageHeader
    [ "Your name entered is: " <> map toUpper name,
      "Continue with this name?"
    ]
    <> formatYN
    <> formatInputInstruction

formatDifficultyMenu :: [String]
formatDifficultyMenu =
  formatPageHeader ["Select Difficulty Level"]
    <> [ colorize Green "[E]" ++ " Easy level",
         colorize Yellow "[N]" ++ " Normal level",
         colorize Pink "[H]" ++ " Hard level",
         emptyLine
       ]
    <> formatOptions

formatGameInterfaceHeader :: String -> Difficulty -> String -> String -> [String]
formatGameInterfaceHeader name diff time et =
  formatPageHeader
    [ "Welcome to Sudoku, " <> map toUpper name,
      verLine,
      leftAlign ("Difficulty Level : " <> show diff),
      leftAlign ("Start Time       : " <> time),
      leftAlign ("Elapsed Time     : " <> et)
    ]

formatGameInterface :: Board -> String -> Difficulty -> String -> String -> [String]
formatGameInterface board name diff time et =
  formatGameInterfaceHeader name diff time et
    <> formatBoard board
    <> [emptyLine]
    <> formatGameOptions
    <> formatInputInstruction

formatSolverInterfaceHeader :: String -> String -> [String]
formatSolverInterfaceHeader name time =
  formatPageHeader
    [ "Welcome to Sudoku Solver, " <> map toUpper name,
      "Start Time: " <> time
    ]

formatSolverInterface :: Board -> String -> String -> [String]
formatSolverInterface board name time =
  formatSolverInterfaceHeader name time
    <> formatBoard board
    <> [emptyLine]
    <> formatSolverOptions
    <> formatInputInstruction

formatSolvedInterface :: Bool -> Board -> String -> Difficulty -> String -> String -> [String]
formatSolvedInterface status board name diff time et =
  if status
    then
      formatGameInterfaceHeader name diff time et
        <> formatSolvedMessage
        <> formatBoard board
        <> [emptyLine]
        <> formatOptions
        <> formatInputInstruction
    else
      formatSolverInterfaceHeader name time
        <> formatSolvedMessage
        <> formatBoard board
        <> [emptyLine]
        <> formatOptions
        <> formatInputInstruction

formatLoadGame :: [String]
formatLoadGame = formatPageHeader ["Load Saved Game"]

formatLoadGameInterface1 :: [String]
formatLoadGameInterface1 =
  formatPageHeader ["Enter a game numebr to load"]

formatLoadGameInterface2 :: [String]
formatLoadGameInterface2 =
  [eqlLine]
    <> formatFilterOptions
    <> [emptyLine]
    <> formatOptions
    <> formatInputInstruction

formatSaveGame :: [String]
formatSaveGame =
  formatPageHeader ["Do you want to save the game?"]
    <> [ colorize Green "[Y]" ++ " Save the game and leave",
         colorize Red "[N]" ++ " Leave without saving"
       ]
    <> formatInputInstruction

formatQuitGame :: [String]
formatQuitGame =
  formatPageHeader ["Are you sure you want to QUIT?"]
    <> formatYN
    <> formatInputInstruction

formatBackToMainMenu :: [String]
formatBackToMainMenu =
  formatPageHeader ["Do you want to go back to MAIN MENU?"]
    <> formatYN
    <> formatInputInstruction

-- Options and Instructions Interfaces

formatOptions :: [String]
formatOptions =
  [ colorize Yellow "[M]" ++ " Main Menu",
    colorize Red "[Q]" ++ " Quit Program"
  ]

formatGameOptions :: [String]
formatGameOptions =
  [ bold "Action Based On Current Location: ",
    colorize Cyan "[W]" ++ "   Move Up",
    colorize Cyan "[A]" ++ "   Move Left",
    colorize Cyan "[S]" ++ "   Move Down",
    colorize Cyan "[D]" ++ "   Move Right",
    colorize Cyan "[NUM]" ++ " Fill in the number at the current location",
    colorize Cyan "[E]" ++ "   Erase the number at the current location",
    emptyLine,
    bold "Quick Actions: (The row and column are 0 based index, ranging from 0 to 8)",
    colorize Cyan "[E ROW COL]" ++ "        Erase By Row and Column",
    colorize Cyan "[F ROW COL VALUE]" ++ "  Fill By Row, Column and Value",
    colorize Cyan "[U]" ++ "                Undo Last Move",
    emptyLine
  ]
    <> formatOptions

formatSolverOptions :: [String]
formatSolverOptions =
  [ bold "Action Based On Current Location: ",
    colorize Cyan "[W]" ++ "    Move Up",
    colorize Cyan "[A]" ++ "    Move Left",
    colorize Cyan "[S]" ++ "    Move Down",
    colorize Cyan "[D]" ++ "    Move Right",
    colorize Cyan "[NUM]" ++ "  Fill in the number at the current location",
    colorize Cyan "[E]" ++ "    Erase the number at the current location",
    emptyLine,
    bold "Quick Actions: (The row and column are 0 based index, ranging from 0 to 8)",
    colorize Cyan "[E ROW COL]" ++ "         Erase By Row and Column",
    colorize Cyan "[F ROW COL VALUE] " ++ "  Fill By Row, Column and Value",
    colorize Cyan "[U]" ++ "                 Undo Last Move",
    colorize Cyan "[P]" ++ "                 Solve the board",
    emptyLine
  ]
    <> formatOptions

formatInputInstruction :: [String]
formatInputInstruction =
  [ eqlLine,
    colorize Cyan "Enter your choice >> "
  ]

formatYN :: [String]
formatYN =
  [ colorize Green "[Y]" ++ " Yes",
    colorize Red "[N]" ++ " No"
  ]

formatFilterOptions :: [String]
formatFilterOptions =
  [ bold "Reset:",
    colorize Green "[A]" ++ "       Show All Records",
    emptyLine,
    bold "Filter Options:",
    colorize Green "[FE]" ++ "      Filter by Easy",
    colorize Green "[FN]" ++ "      Filter by Normal",
    colorize Green "[FH]" ++ "      Filter by Hard",
    colorize Green "[FS]" ++ "      Filter by Solver Mode",
    colorize Green "[FG]" ++ "      Filter by All Difficulty Game Mode",
    colorize Green "[FSOL]" ++ "    Filter by Solved Game",
    colorize Green "[FUNSOL]" ++ "  Filter by Unsolved Game",
    emptyLine,
    bold "Sort Options:",
    colorize Green "[SN]" ++ "      Sort by Name",
    colorize Green "[SS]" ++ "      Sort by Solved Status",
    colorize Green "[SGT]" ++ "     Sort by Generate Time",
    colorize Green "[SET]" ++ "     Sort by Elapsed Time",
    colorize Green "[SLST]" ++ "    Sort by Last Saved Time"
  ]

-- Messages

formatExitMessage :: [String]
formatExitMessage =
  [ colorize Green "Thank you for playing Sudoku!",
    colorize Green "Goodbye!"
  ]

formatSavedMessage :: [String]
formatSavedMessage = formatPageHeader [colorize Green "Game saved successfully!"]

formatNotSavedMessage :: [String]
formatNotSavedMessage = formatPageHeader [colorize Red "Game not saved!"]

formatSolvedMessage :: [String]
formatSolvedMessage = [colorize Green "The board has been solved!"]

formatNoRecord :: [String]
formatNoRecord = [colorize Red "No game records found."]

formatInvalidLoadInput :: [String]
formatInvalidLoadInput = [colorize Red "Invalid input, please enter a valid command or record number."]

formatInvalidInput :: [String]
formatInvalidInput = [colorize Red "Invalid input, please enter a valid command."]

formatInvalidName :: [String]
formatInvalidName = [colorize Red "Invalid name, please enter a name with at most 10 characters."]

formatSolvedFailed :: [String]
formatSolvedFailed = [colorize Red "The board cannot be solved, please check the board again."]

formatFillQuestionErrMsg :: [String]
formatFillQuestionErrMsg = [colorize Red "Cannot overwrite question cell!"]

formatEraseQuestionErrMeg :: [String]
formatEraseQuestionErrMeg = [colorize Red "Cannot erase question cell!"]

formatUndoFailed :: [String]
formatUndoFailed = [colorize Red "Undo failed, cannot undo anymore."]

formatInvalidRow :: [String]
formatInvalidRow = [colorize Red "Invalid row, please enter a row number between 0 and 8."]

formatInvalidCol :: [String]
formatInvalidCol = [colorize Red "Invalid column, please enter a column number between 0 and 8."]

formatInvalidValue :: [String]
formatInvalidValue = [colorize Red "Invalid value, please enter a value between 1 and 9."]

formatInvalidMove :: [String]
formatInvalidMove = [colorize Red "Invalid move, you will exceed the board."]

-- Other Formatting

formatRecordWithStr :: String -> String -> String -> String -> String -> String -> String -> Bool -> [String]
formatRecordWithStr num name diff solvedStr gtStr etStr lstStr status =
  let str =
        printf
          "%-5s %-15s %-13s %-10s %-21s %-15s %-21s"
          ("[" ++ num ++ "]")
          name
          diff
          solvedStr
          gtStr
          etStr
          lstStr
   in if status then [colorize Blue (bold str)] else [str]

formatRecordWithGameState :: TimeZone -> [GameState Board] -> Bool -> [[String]]
formatRecordWithGameState timeZone gameStates headerstatus =
  [ formatRecordWithStr
      (show num)
      (userName gameState)
      (show $ difficulty gameState)
      (show $ solved gameState)
      (formatDateTime timeZone (generateTime gameState))
      (formatElapsedTime (elapsedTime gameState))
      (formatDateTime timeZone (lastSavedTime gameState))
      headerstatus
    | (num, gameState) <- zip [1 :: Int ..] gameStates
  ]
