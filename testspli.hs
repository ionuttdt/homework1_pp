import Data.List
import qualified Data.List as List

string = "class       Int\nclass=Double\nclass String\n\nclass Float\nclass PP\n\n"
--split_s = splitOneOf ":;(,)" "Ana::are;;;Mere(Double,Int)"
--t = putStrLn split_s

data T  = C Double | B String Integer

--f1 = filter odd . map (+1)
--f2 = map (+1) . filter even
--f3 = filter odd $ map (+1)
--f4 = map (+1) $ filter even

noNewLine :: [String] -> [String]
noNewLine [] = []
noNewLine (l:ls) =
    if ( l == "\n")
        then (noNewLine ls)
    else if ((head l) == '\n')
        then ((tail l):(noNewLine ls))
    else (l:(noNewLine ls))

sizeString :: [a] -> Integer
sizeString [] = 0
sizeString (x:xs) = 1 + (sizeString xs) 

noChar2 :: Char -> [String] -> [String]
noChar2 char [] = []
noChar2 char (l:ls) =
    if (l == (char:[]) )
        then (noChar2 char ls)
    else if ((head l) == char)
        then ((tail l):(noChar2 char ls))
    else (l:(noChar2 char ls))

data Test = Va String | Func Integer Test deriving Eq

getType :: Test -> Test -> Bool
getType (Va _) (Va _) = True
getType (Func _ _) (Va _) = False
getType (Va _) (Func _ _) = False
getType (Func _ _) (Func _ _) = True 

wordsBy2 :: Char -> String -> [String]
wordsBy2 char str = noChar2 char ( List.groupBy (\x y -> y /= char) str)

noChar3 :: String -> String -> String
noChar3 [] str = str
noChar3 del str =
    if (((head del):[]) == str) || ((head del) == (head str))
        then tail str
    else noChar3 (tail del) str

voidStr :: [String] -> [String]
voidStr [] = []
voidStr (x:xs) =
    if x == ""
        then voidStr xs
    else x:(voidStr xs)


tipF :: [String] -> String -> Bool
tipF [] str = False
tipF l str =
    if str == (head l)
        then True
    else tipF (tail l) str
--["Double","B::minus(Double,","Int)"]

wordsBy3 :: String -> String -> [String]
wordsBy3 del str =  ( List.groupBy (\x y -> (existDel y del) ) str)

existDel :: Char -> String -> Bool
existDel char [] = True
existDel char del =
    if char == (head del)
        then False
    else existDel char (tail del)

wordsBy str = noNewLine ( List.groupBy (\x y -> y /= '\n') str )
