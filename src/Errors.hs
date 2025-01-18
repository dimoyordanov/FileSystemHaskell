module Errors where

data Error = 
    FileNotFound
    | DirectoryNotFound
    | FileIsDirectory
    | TooManyFiles
    | DirectoryIsFile
    | FailedParsingCommand
    | FailingReadingFromStdin
    | FailedToCreateFile
    | FailedToCreateDir
    | FailedGettingState
    | FailedNavigation
    | FailedListingTheFiles
    | SomethingIsWrongParsingTheFile
    | Error
    deriving (Eq)

instance Show Error where
    show Error = "Error: error "
    show SomethingIsWrongParsingTheFile = "Error: failed parsing the command "
    show FailedNavigation = "Erorr: Failed navigating files "
    show FailedListingTheFiles = "Erorr: Failed listing the files " 
    show FailedGettingState = "Error: Failed getting state "
    show FailedToCreateDir = "Error: Failed to create dir "
    show FailedToCreateFile = "Error: Failed to create dir "
    show FailingReadingFromStdin = "Error: Reading from stdin "
    show FileNotFound = "Error: File not found "
    show DirectoryNotFound = "Error: Directory is not found "
    show FileIsDirectory = "Error: File is directory "
    show TooManyFiles = "Error: Too many files "
    show DirectoryIsFile = "Error: Directory is file "
    show FailedParsingCommand = "Error: Failed parsing command "