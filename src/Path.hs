module Path where

type Path = [String]

combinePath :: Path -> Path -> Path
combinePath p1 p2 = p1 ++ p2

pathToString :: Path -> String
pathToString = ('/':) . foldl (\x y -> x ++ y ++ "/") ""

filterFilePath :: Path -> Path
filterFilePath a = filter (/= "..") (foldr step [] a)
                where 
                    step _ ("..": xs) = xs
                    step next l = next: l

stringToPath :: String -> Path
stringToPath path = filter (/="") $ foldr step [""] path
                    where 
                        step _ [] = error "problem with foldr"
                        step '/' combined = "": combined
                        step x (c: cs) = (x: c) : cs

-- Exports Path, pathToString, filterFilePath, stringToPath, combinePath