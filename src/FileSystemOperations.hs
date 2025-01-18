module FileSystemOperations where

import Path (Path)
import FileSystemStructures (Directory(..), File(..), Contents(Dir, FileEntry), filterDirectoryContentsByNameNot, filterDirectoryContentsByName, getNameContents)
import Errors  (Error(..))

locateDirectory :: Contents -> Path -> Either String Contents
locateDirectory struct@(Dir _) [] = Right struct
locateDirectory struct@(Dir _) [""] = Right struct
locateDirectory (Dir dsPar) (fr: end) = case filterDirectoryContentsByName fr (directoryEntries dsPar) of
                                            [] -> Left $ show DirectoryNotFound ++ fr
                                            [dsChild] -> locateDirectory dsChild end
                                            _ -> Left $ show TooManyFiles ++ fr 

locateDirectory (FileEntry val) _ = Left $ show DirectoryIsFile ++ fileName val


getFileContent :: Directory -> Path -> Either String String
getFileContent _ [] = Left $ show Error
getFileContent struct [x] = case filterDirectoryContentsByName x (directoryEntries struct) of
                                [FileEntry fs] -> Right (fileContents fs)
                                _ -> Left $ show FileNotFound
getFileContent struct (x: xs) = case filterDirectoryContentsByName x (directoryEntries struct) of
                                    [Dir ds] -> getFileContent ds xs
                                    _ -> Left $ show FileNotFound


addContent :: Directory -> Path -> Contents -> Either String Directory
addContent struct [] cont = if any ((==getNameContents cont).getNameContents) (directoryEntries struct)
     then Left $ show TooManyFiles ++ getNameContents cont 
     else Right $ Directory (directoryName struct) ( cont: directoryEntries struct) 
addContent struct (fr: end) cont = case filterDirectoryContentsByName fr (directoryEntries struct) of
    [Dir ds] ->  
        (\ a
            -> Right
                    $ Directory
                        (directoryName struct)
                        (Dir a
                        : filterDirectoryContentsByNameNot fr (directoryEntries struct)))
            =<< addContent ds end cont
    _ -> Left $ show DirectoryNotFound ++ fr

removeContent :: Directory -> Path -> Either String Directory
removeContent _ [] = Left $ show FileNotFound
removeContent struct [x]=  Right $ Directory (directoryName struct) (filterDirectoryContentsByNameNot x (directoryEntries struct))
removeContent struct (fr: end) = 
    case filterDirectoryContentsByName fr (directoryEntries struct) of 
        [Dir ds] -> 
            (\ a
                -> Right
                        $ Directory
                            (directoryName struct)
                            (Dir a
                            : filterDirectoryContentsByNameNot fr (directoryEntries struct)))
                =<< removeContent ds end
        _ -> Left $ show DirectoryNotFound ++ fr


listDirectory :: Directory -> String
listDirectory a = drop 1 $ concatMap (("\n"++).getNameContents) $ directoryEntries a


-- exports removeContent, addContent, getFileContent, locateDirectory