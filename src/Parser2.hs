{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Parser2 where

import Data.Char (isSpace, isLetter, isNumber)
import Control.Applicative (liftA2, Alternative((<|>), empty, many, some))
import Commands (CommandsTerminal(..))
import Errors (Error(SomethingIsWrongParsingTheFile))
import Data.Maybe (fromMaybe)

parseCommand :: String -> CommandsTerminal
parseCommand = fromMaybe (Print "Error: parsing after unwraping commands terminal"). parseCommandTerminal


parseCommandTerminal :: String -> Maybe CommandsTerminal
parseCommandTerminal =  parse commandCreator . dropWhile (==' '). reverse.dropWhile (==' ').reverse

parseCommandError :: String -> String
parseCommandError  = (show SomethingIsWrongParsingTheFile++)


-- Взето от: https://github.com/fmi-fp-lab/fp-lab-2024-25/blob/master/exercises/09/Parser.hs
-- START

newtype Parser a = MkParser {runParser :: String -> [(String, a)]}

parse :: Parser a -> String -> Maybe a
parse px str =
  case runParser px str of
    [] -> Nothing
    (_, x) : _ -> Just x

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f px = MkParser $ \str ->
    [(str2, f res) | (str2, res)<-runParser px str]

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = MkParser $ \str -> [(str, x)]

  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 f px py= MkParser $ \str -> [
        (rest2, f x y) |
        (rest1,x) <- runParser px str,
        (rest2, y) <- runParser py rest1]

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) px f =
        MkParser $ \str ->
            [ (rest2, y)
            | (rest1, x) <- runParser px str
            , (rest2, y) <- runParser (f x) rest1
            ]

parseFailure :: Parser a
parseFailure = MkParser $ const []

instance Alternative Parser where
  empty :: Parser a
  empty = parseFailure

  (<|>) :: Parser a -> Parser a -> Parser a
  px <|> py =
    MkParser $ \str -> runParser px str ++ runParser py str

-- END

char :: (Char->Bool) -> Parser Char
char c = MkParser $ \str -> case str of
                        [] -> []
                        (x: xs) -> ([(xs, x) | c x])

charAdvanced :: (Char->Bool) -> Parser (Maybe Char)
charAdvanced c = MkParser $ \str -> case str of
                        [] -> []
                        (x: xs) -> if c x then [(xs, Just x)] else [(str, Nothing)]

path :: Parser [Char]
path = some $ char isLetter <|> char isNumber <|> char (`elem` ['-', '_', '.', '/'])

string :: Parser [Char]
string = some $ char isLetter <|> char isNumber <|> char (`elem` ['-', '_', '.'])

space :: Parser Char
space = char isSpace

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa ps = some (ps *> pa)


sepAfter :: Parser a -> Parser sep -> Parser [a]
sepAfter pa ps = do
  start <- pa
  end <- sepBy pa ps
  return $ start: end


pathParser :: Parser (Bool, [[Char]])
pathParser = do 
  let separator = char (=='/') in
    ((True, ) <$> sepAfter string separator) <|> ((False, ) <$> sepBy string separator) <|> ((True,).(:[]) <$> string) <|> ((False, []) <$ separator) <|> ((False, ).(:[]).(:[]) <$> char (=='>'))
  

commandParser :: Parser ([Char], [(Bool, [[Char]])])
commandParser = do
    (,) <$> string <*> many (some space *> pathParser)


commandCreator :: Parser CommandsTerminal
commandCreator = do
  parsedValue <- commandParser
  return $ case parsedValue of
        ("ls", [(True, x)]) -> ListFilesRelative x
        ("ls", [(False, x)]) -> ListFilesAbsolute x
        ("ls", []) -> ListFilesLocal
        ("pwd", []) -> PrintWorkingDirectory
        ("cd", [(True, x)]) -> ChangeDirRelative x 
        ("cd", [(False, x)]) -> ChangeDirAbsolute x 
        ("cat", args) -> parseCat args
        ("rm", args) -> parseRm args
        ("mkdir", [(True, x)]) -> MakeDirectoryLocal x
        ("mkdir", [(False, x)]) -> MakeDirectory x
        ("quit", _) -> Quit
        _ -> Print $ parseCommandError ""
     where 
        parseCat args = if any fst args then  Print $ parseCommandError "cat" else Concatenate (map snd args) Nothing
        parseRm args = if any fst args then  Print $ parseCommandError "rm"  else Remove $ map snd args
