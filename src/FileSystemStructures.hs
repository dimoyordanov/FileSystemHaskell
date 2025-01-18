module FileSystemStructures(Directory(..), File(..), Contents(..), filterDirectoryContentsByName, filterDirectoryContentsByNameNot, getNameContents, listDirectory) where

-- implement show directory

data Directory = Directory{
    directoryName :: String,
    directoryEntries :: [Contents]
} deriving (Eq)

data File = File {
    fileName :: String,
    fileContents :: String
} deriving (Show, Eq)

data Contents = 
    Dir Directory
    | FileEntry File
     deriving (Show, Eq)

instance Show Directory where
    show a 
        | null (directoryEntries a) = ""
        | otherwise = drop 1 $ concatMap (("\n"++).getNameContents) $ directoryEntries a

getNameContents :: Contents -> String
getNameContents (Dir str) = directoryName str
getNameContents (FileEntry fstr) = fileName fstr

filterDirectoryContentsByNameAbs :: (String -> Bool) -> [Contents] ->[Contents]
filterDirectoryContentsByNameAbs eqvv = filter (eqvv.getNameContents)                            

filterDirectoryContentsByName :: String -> [Contents] ->[Contents]
filterDirectoryContentsByName name = filterDirectoryContentsByNameAbs (==name)

filterDirectoryContentsByNameNot :: String -> [Contents] ->[Contents]
filterDirectoryContentsByNameNot name = filterDirectoryContentsByNameAbs (/=name)

listDirectory :: Directory -> String
listDirectory = show
   


-- exports Directory, File, Contents, filterDirectoryContentsByNameNot, filterDirectoryContentsByName, getNameContents