{-# LANGUAGE TupleSections #-}

module Commands(CommandsTerminal(..), executeCommand) where

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)

import FileSystemStructures (Directory(..), File(..), Contents(Dir, FileEntry), listDirectory)
import ProgramState (ProgramState, ProgramStateError)
import FileSystemOperations (removeContent, addContent, getFileContent, locateDirectory)
import Path (Path, filterFilePath, combinePath, pathToString)
import Errors (Error(..))

executeCommand :: Directory -> Path -> CommandsTerminal -> ProgramState
executeCommand dir p =  execute dir $ filterFilePath p

data CommandsTerminal
    = ListFilesRelative Path
    | ListFilesAbsolute Path 
    | ListFilesLocal
    | PrintWorkingDirectory 
    | ChangeDirRelative Path
    | ChangeDirAbsolute Path
    | Concatenate [Path] (Maybe String)
    | Remove [Path]
    | MakeDirectory Path
    | MakeDirectoryLocal Path
    | Quit
    | Print String
    deriving (Show, Eq)

execute :: Directory -> Path -> CommandsTerminal -> ProgramState
execute dir p PrintWorkingDirectory = (p, pathToString p, dir)
execute dir p t1@(MakeDirectory _) = makeDirectory dir p t1
execute dir p (MakeDirectoryLocal path) = makeDirectory dir p (MakeDirectory (combinePath p path) )
execute dir p ListFilesLocal = executeLsCommand dir p (ListFilesAbsolute p)
execute dir p t1@(ListFilesAbsolute _) = executeLsCommand dir p t1
execute dir p (ListFilesRelative path2) = executeLsCommand dir p (ListFilesAbsolute (combinePath p path2))
execute dir p t1@(ChangeDirAbsolute _1) = changeDirectoryAbsolute dir p t1
execute dir p (ChangeDirRelative path) = if path == [".."] then changeDirectoryAbsolute dir p (ChangeDirAbsolute $ reverse $ drop 1 $ reverse p) else changeDirectoryAbsolute dir p (ChangeDirAbsolute $ filterFilePath $ combinePath p path)                                             
execute dir p (Concatenate [[">"]] string) = (p, fromMaybe (show FailingReadingFromStdin) string, dir)
execute dir p (Concatenate [path] _) = (p, either id id $ getFileContent dir path, dir)
execute dir p (Concatenate [[">"], path] string) = case reverse path of
        [] -> errorMessage
        (x: xs) -> either (p,,dir) (p, "File created",) $ addContent dir (reverse xs) (FileEntry $ File x (fromMaybe "" string))
        where
            errorMessage = (p, show FailedToCreateFile, dir) 
execute dir p (Concatenate l _) = case reverse l of
        (x: (y :xs)) -> if y == [">"] -- x imeto na fajla deto she suzdavame
            then case reverse x of -- reverse x toest purvo imeto posle putq
                        [] -> errorMessage
                        (x1: xs1) -> either (p,,dir) (p, "File created",) $ addContent dir (reverse xs1) (FileEntry $ File x1 (concatenateText dir xs))
            else 
                (p, concatenateText dir l, dir)
        _ -> errorMessage
        where 
            errorMessage = (p, show FailedToCreateFile, dir)
execute dir p (Remove l) = either errorMessage  (p, "Files successfuly removed",) $ foldr step (Right dir) l
                        where 
                            errorMessage = (p, , dir)
                            step :: Path -> Either String Directory -> Either String Directory 
                            step x y = (`removeContent` x) =<< y
execute dir p (Print a) = (p, a, dir)
execute dir p Quit = (p, "Quiting application", dir)


errorMessageLS :: Path -> Directory -> (String -> ProgramState)
errorMessageLS p dir = (p, , dir) 

errorMessageChangeDir :: Path -> Directory -> (String -> ProgramState)
errorMessageChangeDir p dir = (p, , dir) 

unwrapFileLs :: (String -> ProgramStateError) -> Path -> Directory -> Contents -> ProgramState
unwrapFileLs _ p dir (Dir a) = (p, listDirectory a, dir)
unwrapFileLs e _ _ _ = e $ show FailedGettingState

executeLsCommand :: Directory -> Path -> CommandsTerminal -> ProgramState
executeLsCommand dir p (ListFilesAbsolute path2) = either err (unwrapFileLs err p dir) $ locateDirectory (Dir dir) path
                                            where
                                                path = filterFilePath path2
                                                err = errorMessageLS p dir
executeLsCommand dir p _ = errorMessageLS p dir $ show FailedListingTheFiles

concatenateText :: Directory -> [Path] -> String
concatenateText dir = foldr (\path string  -> fromRight "" (getFileContent dir path) ++ string) ""

unwrapFileCd :: (String -> ProgramStateError) -> Path -> Directory -> Contents -> ProgramState
unwrapFileCd _ p dir (Dir _) = (p, "Successfuly moved to new directory", dir)
unwrapFileCd e _ _ _ = e $ show FailedNavigation

makeDirectory :: Directory -> Path -> CommandsTerminal -> ProgramState
makeDirectory dir p (MakeDirectory path) = case  reverse path of
    [] -> (p, show FailedToCreateDir, dir)
    (x: xs) -> either (p,,dir) (p, "Created directory successfuly",) $ addContent dir (reverse xs) (Dir $ Directory x [])
makeDirectory dir p _ = (p, show FailedToCreateDir, dir)

changeDirectoryAbsolute :: Directory -> Path -> CommandsTerminal -> ProgramState
changeDirectoryAbsolute dir p (ChangeDirAbsolute path2) = either err (unwrapFileCd err path2 dir) $ locateDirectory (Dir dir) path
                                            where 
                                                path = filterFilePath path2
                                                err = errorMessageChangeDir p dir
changeDirectoryAbsolute dir p _ = errorMessageChangeDir p dir $ show FailedNavigation
