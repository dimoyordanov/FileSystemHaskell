module Parser(parseCommand) where

import Commands (CommandsTerminal(..))
import Path (stringToPath)
import Errors (Error(SomethingIsWrongParsingTheFile))

parseCommand :: String -> CommandsTerminal
parseCommand = parseTokenizedString


tokenizeCommand :: String -> [] String
tokenizeCommand =  words

parseCommandError :: String -> String
parseCommandError  = (show SomethingIsWrongParsingTheFile++)

parseTokenizedString :: String -> CommandsTerminal
parseTokenizedString userInput =
    case tokenizeCommand userInput of
     [] -> Print ""
     (l : args) -> 
        case (l, length args) of
        ("ls", 1) -> parseLs args
        ("ls", 0) -> ListFilesLocal
        ("pwd", _) -> PrintWorkingDirectory
        ("cd", 1) -> parseCd args
        ("cat", m) -> if m >= 0 then Concatenate (map stringToPath args) Nothing else Print $ parseCommandError "cat"
        ("rm", m) -> if m >= 0 then Remove (map stringToPath args) else Print $ parseCommandError "rm"
        ("mkdir", 1) -> parseMkdir args
        ("quit", _) -> Quit
        _ -> Print $ parseCommandError ""
     where 
        parseLs args = case args of 
                [z] -> case z of
                    x: xs -> if x == '/' then ListFilesAbsolute (stringToPath xs) else ListFilesRelative  $ stringToPath $ x:xs
                    _ -> Print $ parseCommandError "ls"
                _ -> Print $ parseCommandError "ls"
        parseCd args = case args of 
                [z] -> case z of
                    x: xs -> if x == '/' then ChangeDirAbsolute (stringToPath xs) else ChangeDirRelative  $ stringToPath $ x:xs
                    _ -> Print $ parseCommandError "cd"
                _ -> Print $ parseCommandError "cd"
        parseMkdir args = case args of
            [] -> Print $ parseCommandError "mkdir"
            [x] -> case x of
                "" -> Print$ parseCommandError "mkdir"
                '/': xs -> MakeDirectory $ stringToPath xs
                str -> MakeDirectoryLocal $ stringToPath str
            _ -> Print$ parseCommandError "mkdir"
