module Main where

import Test.Hspec
import ProgramState (ProgramState)
import Quit (checkQuitApplication)
import RootDirectory (rootDirectory)
import Path ( Path, pathToString, filterFilePath, stringToPath, combinePath)
import qualified Parser as T (parseCommand)
import qualified Parser2 as B (parseCommand)
import Commands (CommandsTerminal(..), executeCommand)

import FileSystemStructures (Directory(Directory), getNameContents, Contents (FileEntry, Dir), filterDirectoryContentsByName, File (File))
import FileSystemOperations (listDirectory, locateDirectory, getFileContent, addContent)
import Test.Hspec (shouldBe)

spec :: Spec
spec = do
    describe "quitStateNegative" $ do
        it "Should return nothing when in quit state" $ do
            checkQuitApplication ([], "Quiting application", rootDirectory) `shouldBe` Nothing
    
    describe "quitStatePositive" $ do
        it "Should return just when not in quit state" $ do
            checkQuitApplication ([], "Bazinga", rootDirectory) `shouldBe` Just ([], "Bazinga", rootDirectory)
    

    describe "checkPathToString function" $ do
        it "Check root only /" $ do
            pathToString [] `shouldBe` "/"
        it "Check one folder in" $ do
            pathToString ["hello"] `shouldBe` "/hello/"
        it "Check many folder in" $ do
            pathToString ["hello", "world"] `shouldBe` "/hello/world/"

    describe "check fileterFilePath function" $ do
        it "check only simple" $ do
            filterFilePath ["hello" , ".."] `shouldBe` []
        it "check only simple" $ do
            filterFilePath ["hello" , "..", "world"] `shouldBe` ["world"]
        it "check only simple" $ do
            filterFilePath [".."] `shouldBe` [] 

    describe "check string to path function" $ do
        it "check root" $ do
            stringToPath "/" `shouldBe` []
        it "check inside one" $ do
            stringToPath "/hello/" `shouldBe` ["hello"]
        it "check inside many" $ do
            stringToPath "/hello/world/123" `shouldBe` ["hello", "world", "123"]
        it "check inside many 2" $ do
            stringToPath "/hello/world/123/" `shouldBe` ["hello", "world", "123"]
  
    describe "check parse of text into tokens" $ do
        it "check parseed empty" $ do
            T.parseCommand "" `shouldBe` Print ""
        it "check parse basic" $ do
            T.parseCommand "123 123" `shouldBe` Print  "Error: failed parsing the command "
        it "check parse ls 1" $ do
            T.parseCommand "ls" `shouldBe` ListFilesLocal
        it "check parse ls 2" $ do
            T.parseCommand "ls /123/123" `shouldBe` ListFilesAbsolute ["123","123"]
        it "check parse ls 3" $ do
            T.parseCommand "ls 123/123" `shouldBe` ListFilesRelative ["123", "123"]
        it "check parse ls wrong" $ do
            T.parseCommand "ls 123 321" `shouldBe` Print  "Error: failed parsing the command "
        it "check parse pwd" $ do
            T.parseCommand "pwd" `shouldBe` PrintWorkingDirectory
        it "check parse cd 1" $ do
            T.parseCommand "cd /123/322" `shouldBe` ChangeDirAbsolute ["123", "322"]
        it "check parse cd 2" $ do
            T.parseCommand "cd 123/322" `shouldBe` ChangeDirRelative ["123", "322"]
        it "check parse cd wrong" $ do
            T.parseCommand "cd 1111 11111" `shouldBe` Print  "Error: failed parsing the command "
        it "check parse cd wrong" $ do
            T.parseCommand "cd  " `shouldBe` Print  "Error: failed parsing the command "
        it "check parse rm" $ do
            T.parseCommand "rm 123" `shouldBe` Remove [["123"]]
        it "check parse rm more" $ do
            T.parseCommand "rm 123 /321/456" `shouldBe` Remove [["123"] , ["321", "456"]]
        it "check parse cat" $ do
            T.parseCommand "cat baz > 123" `shouldBe` Concatenate [["baz"], [">"], ["123"]] Nothing
        it "check parse cat more" $ do
            T.parseCommand "cat >" `shouldBe` Concatenate [[">"]] Nothing
        it "check parse mkdir" $ do
            T.parseCommand "mkdir /123/321" `shouldBe` MakeDirectory ["123","321"]
        it "check parse mkdir rel" $ do
            T.parseCommand "mkdir 123/321" `shouldBe` MakeDirectoryLocal ["123","321"]
        it "check parse mkdir wrong" $ do
            T.parseCommand "mkdir 1233 3213123" `shouldBe` Print "Error: failed parsing the command "
        it "check parse quit" $ do
            T.parseCommand "quit" `shouldBe` Quit
    
    describe "check parse of text into tokens" $ do
        it "check parseed empty" $ do
            B.parseCommand "" `shouldBe` Print "Error: parsing after unwraping commands terminal"
        it "check parse basic" $ do
            B.parseCommand "123 123" `shouldBe` Print  "Error: failed parsing the command "
        it "check parse ls 1" $ do
            B.parseCommand "ls" `shouldBe` ListFilesLocal
        it "check parse ls 2" $ do
            B.parseCommand "ls /123/123" `shouldBe` ListFilesAbsolute ["123","123"]
        it "check parse ls 3" $ do
            B.parseCommand "ls 123/123" `shouldBe` ListFilesRelative ["123", "123"]
        it "check parse ls wrong" $ do
            B.parseCommand "ls 123 321" `shouldBe` Print  "Error: failed parsing the command "
        it "check parse pwd" $ do
            B.parseCommand "pwd" `shouldBe` PrintWorkingDirectory
        it "check parse cd 1" $ do
            B.parseCommand "cd /123/322" `shouldBe` ChangeDirAbsolute ["123", "322"]
        it "check parse cd 2" $ do
            B.parseCommand "cd 123/322" `shouldBe` ChangeDirRelative ["123", "322"]
        it "check parse cd wrong" $ do
            B.parseCommand "cd 1111 11111" `shouldBe` Print  "Error: failed parsing the command "
        it "check parse cd wrong" $ do
            B.parseCommand "cd  " `shouldBe` Print  "Error: failed parsing the command "
        it "check parse rm" $ do
            B.parseCommand "rm /123" `shouldBe` Remove [["123"]]
        it "check parse rm more" $ do
            B.parseCommand "rm /123 /321/456" `shouldBe` Remove [["123"] , ["321", "456"]]
        it "check parse cat" $ do
            B.parseCommand "cat /baz > /123" `shouldBe` Concatenate [["baz"], [">"], ["123"]] Nothing
        it "check parse cat more" $ do
            B.parseCommand "cat >" `shouldBe` Concatenate [[">"]] Nothing
        it "check parse mkdir" $ do
            B.parseCommand "mkdir /123/321" `shouldBe` MakeDirectory ["123","321"]
        it "check parse mkdir rel" $ do
            B.parseCommand "mkdir 123/321" `shouldBe` MakeDirectoryLocal ["123","321"]
        it "check parse mkdir wrong" $ do
            B.parseCommand "mkdir 1233 3213123" `shouldBe` Print "Error: failed parsing the command "
        it "check parse quit" $ do
            B.parseCommand "quit" `shouldBe` Quit

    describe "File system functions" $ do
        it "check Get name directoru" $ do
            getNameContents (Dir ( Directory "Bazinga" [])) `shouldBe` "Bazinga"
        
        it "check Get name file" $ do
            getNameContents (FileEntry (File "Bazinga" [])) `shouldBe` "Bazinga"

    describe "Filter directories by some name" $ do
        it "Filter directories" $ do
            filterDirectoryContentsByName "test" [Dir $ Directory "test" []] `shouldBe` [Dir $ Directory "test" []]
        it "Filter directories2" $ do
            filterDirectoryContentsByName "test" [Dir $ Directory "test0" [], Dir $ Directory "test" [], FileEntry $ File "test2" []] `shouldBe` [Dir $ Directory "test" []]

    describe "List directories" $ do
        it "List directories empty" $ do
            listDirectory (Directory "test" []) `shouldBe` []
        it "List directories one" $ do
            listDirectory (Directory "test" [FileEntry $ File "one" ""]) `shouldBe` "one"
        it "List directories two" $ do
            listDirectory (Directory "test" [FileEntry $ File "one" "",FileEntry $ File "two" ""]) `shouldBe` "one\ntwo"

    describe "locateDirectory" $ do
        it "locate directory location empty" $ do
            locateDirectory (Dir $ Directory "" []) [] `shouldBe` Right (Dir $ Directory "" [])
        it "locate directory location one" $ do
            locateDirectory (Dir $ Directory "" [Dir $ Directory "Baz" []]) ["Baz"] `shouldBe` Right (Dir $ Directory "Baz" [])
        it "locate directory location two" $ do
            locateDirectory (Dir $ Directory "" [Dir $ Directory "Baz2" [], Dir $ Directory "Baz" [Dir $ Directory "Fuz" [] ]]) ["Baz", "Fuz"] `shouldBe` Right (Dir $ Directory "Fuz" [])
        it "locate directory wrong" $ do
            locateDirectory (FileEntry $ File "Baz" "") [] `shouldBe` Left "Error: Directory is file Baz"

    describe "Get file content" $ do
        it "Get file content" $ do
            getFileContent (Directory ""  [FileEntry $ File "Baz" "123"]) ["Baz"] `shouldBe` Right "123"
        it "Get file content2" $ do
            getFileContent (Directory ""  [Dir $ Directory "123"  [FileEntry $ File "Baz" "123"]]) ["123", "Baz"] `shouldBe` Right "123"
        it "Get file content3" $ do
            getFileContent (Directory ""  [Dir $ Directory "123"  [Dir $ Directory "123" [FileEntry $ File "Baz" "123"]]]) ["123", "123", "Baz"] `shouldBe` Right "123"

    describe "Test command" $ do
        it "Execute command" $ do
            executeCommand rootDirectory [] PrintWorkingDirectory `shouldBe` ([], "/", rootDirectory)
        it "Execute command more" $ do
            executeCommand rootDirectory ["hello"] PrintWorkingDirectory `shouldBe` (["hello"], "/hello/", rootDirectory)
        it "Make directory" $ do
            executeCommand rootDirectory [] (MakeDirectory ["123"]) `shouldBe` ([], "Created directory successfuly", Directory "" [Dir $ Directory "123" []])
        it "Make directory" $ do
            executeCommand (Directory "" [Dir $ Directory "123" []]) [] (MakeDirectory ["123", "123"]) `shouldBe` ([], "Created directory successfuly", Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]])
        it "Make directory relative" $ do
            executeCommand (Directory "" [Dir $ Directory "123" []]) ["123"] (MakeDirectoryLocal ["123"]) `shouldBe` (["123"], "Created directory successfuly", Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]])
        it "List directory absolute" $ do
            executeCommand (Directory "" [Dir $ Directory "123" []]) [] (ListFilesAbsolute ["123"]) `shouldBe` ([], "", Directory "" [Dir $ Directory "123" []])
        it "List directory absolute 2" $ do
            executeCommand (Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]]) ["123"] (ListFilesAbsolute ["123"]) `shouldBe` (["123"], "123", Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]])
        it "List directory relative" $ do
            executeCommand (Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]]) ["123"] (ListFilesRelative ["123"]) `shouldBe` (["123"], "", Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]])
        it "List directory 1234" $ do
            executeCommand (Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]]) ["1234"] (ListFilesRelative ["123"]) `shouldNotBe` (["123"], "", Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]])
        it "Change directory relative" $ do
            executeCommand (Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]]) ["123"] (ChangeDirRelative ["123"]) `shouldBe` (["123", "123"], "Successfuly moved to new directory", Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]])
        it "Change directory absolute" $ do
            executeCommand (Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]]) [] (ChangeDirAbsolute ["123", "123"]) `shouldBe` (["123", "123"], "Successfuly moved to new directory", Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]])
        it "Change directory wrong" $ do
            executeCommand (Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]]) ["1234"] (ListFilesRelative ["123"]) `shouldNotBe` (["123"], "", Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]])
        it "Concatenate" $ do
            executeCommand (Directory "" [Dir $ Directory "123" [Dir $ Directory "123" []]]) [] (Concatenate [[">"], ["main.txt"]] $ Just "intluce") `shouldBe` ([], "File created", (Directory "" [FileEntry $ File "main.txt" "intluce",Dir $ Directory "123" [Dir $ Directory "123" []]]))

main :: IO ()
main = hspec $ do
    spec