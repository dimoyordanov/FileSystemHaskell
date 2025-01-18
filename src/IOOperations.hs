module IOOperations(takeUserInput) where

import System.IO (hFlush, stdout)
import Commands (executeCommand, CommandsTerminal(Concatenate))
import ProgramState (ProgramState)
import Quit(checkQuitApplication)
import Parser2 (parseCommand)
import Control.Monad ((>=>))

readInputUntillDot :: IO String
readInputUntillDot = fmap (reverse .drop 3. reverse) (readInputUntillDot' "")
                        where 
                            readInputUntillDot' "." = return ""
                            readInputUntillDot' _ =  getLine >>= \x -> ((++) x "\n" ++) <$> readInputUntillDot' x



checkIfCatAndGetUserInput :: CommandsTerminal -> IO CommandsTerminal
checkIfCatAndGetUserInput t1@(Concatenate [[x]] _) = if x==">" then do Concatenate [[x]] . Just <$> readInputUntillDot else return t1
checkIfCatAndGetUserInput t1@(Concatenate [[x], y] _) = if x==">" then do Concatenate [[x], y] . Just <$> readInputUntillDot else return t1
checkIfCatAndGetUserInput l = return l

takeUserInput :: ProgramState -> IO ProgramState
takeUserInput (path, str, dir) = do
    putStr $ str ++ "\n>> "
    hFlush stdout
    
    let 
        parseInput  = (checkIfCatAndGetUserInput . parseCommand) 
        execute token = let stateExecute =  executeCommand dir path token
                in  maybe (return stateExecute) takeUserInput (checkQuitApplication stateExecute)
        absoluteexecutable = parseInput >=> execute
        in getLine >>= absoluteexecutable
