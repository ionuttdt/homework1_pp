{-  
    DUTA VIOREL-IONUT, 321CB
    Tema 1 PP
-}
{-
    Am folosit exagerat de multe functii auxiliare (unele se repeta intr-o anumita
masura), acest lucru fiind un rezultat al modului in care mi-am structurat
subpunctele (le am gandit separat, nu am refolosit prea mult cod).
    Va multumesc pentru intelegere!  
-}

module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState
import Data.List (sort)
import qualified Data.List as List

-- Definire Program si Instruction
data Program = Program [ClassState]
data Instruction = Instruction [String]

initEmptyProgram :: Program
initEmptyProgram = Program [initGlobalClass]

getClasses :: Program -> [String]
getClasses (Program []) = []
getClasses (Program l) = (numeClasa (head l)):(getClasses (Program (tail l)))

getVars :: Program -> [[String]]
getVars (Program []) = []
getVars (Program l) =
    if (numeClasa (head l)) == "Global"
        then (var (head l))
    else (getVars (Program (tail l)))

getParentClass :: String -> Program -> String
getParentClass numeCl (Program []) = ""
getParentClass numeCl (Program l) =
    if ( numeClasa (head l) ) == numeCl 
        then ( clasaParinte (head l) )
    else (getParentClass numeCl (Program (tail l)))

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass numeCl (Program []) = []
getFuncsForClass numeCl (Program l) =
    if ( numeClasa (head l) ) == numeCl
        then ( func (head l) )
    else (getFuncsForClass numeCl (Program (tail l)))

parse :: String -> [Instruction]
parse file = map (\x -> Instruction x)  (map words (wordsBy file))

interpret :: Instruction -> Program -> Program
interpret (Instruction instr) (Program prog) =
    if ((head instr) == "class")
        then if ((sizeString instr) == 2)
            then (newClass (instr!!1) "Global" (Program prog))
        else (newClass (instr!!1) (instr!!3) (Program prog))
    else if (((head instr) == "newvar") && (sizeString instr) == 4)
        then if (tipVar (instr!!3) (Program prog))
            then Program (addVar (instr!!1) (instr!!3) prog)
        else (Program prog)
    else if ((head instr) == "newvar")
        then Program (addVar2 instr prog )
    else Program (addFunc ( splitF) prog)
        where
            splitF = voidStr (map (noChar3 ":() ,") (foldl (++) 
                [] (map (wordsBy3 ":() ,") instr)))

infer :: Expr -> Program -> Maybe String
infer expr prog = inferAux expr prog []

{--======================= FUNCTII AUXILIARE INFER =========================--}
-- functie cu care fac evaluarea propriu-zisa a arborelui de expresii
inferAux :: Expr -> Program -> [String] -> Maybe String
inferAux (Va s) prog aux = getVarC s (getGlobalV prog)
inferAux (FCall varC numF []) prog aux = evalFunc aux varC numF prog
inferAux (FCall varC numF expr) prog aux =
    if eqTypeExpr (head expr) (Va "a")
        then if addVA == "Nothing"
            then Nothing
        else inferAux (FCall varC numF (tail expr)) prog (aux ++ ((addVA):[]))
    else if addFUNC == "Nothing"
        then Nothing
    else inferAux (FCall varC numF (tail expr)) prog (aux ++ (addFUNC:[]))
    where
        addVA = strMaybe (getVarC (strVa (head expr)) (getGlobalV prog) )
        addFUNC = strMaybe (inferAux (head expr) prog [])

-- functie care intoarce toate variabilele din Program
getGlobalV :: Program -> [[String]]
getGlobalV (Program []) = []
getGlobalV (Program prog) =
    if (numeClasa (head prog)) == "Global"
        then (var (head prog))
    else getGlobalV (Program (tail prog))

-- functie folosita pentru a afla clasa corespunzatoare unei variabile data ca
-- parametru; in Expr nu primesc clasa unei functii, ci o variabila corespunzatoar
getVarC :: String -> [[String]] -> Maybe String
getVarC varC [] = Nothing
getVarC varC lvar =
    if varC == (head (head lvar))
        then Just ((head lvar)!!1)
    else getVarC varC (tail lvar)

-- primeste ca parametru un nume si intoarce clasa respectiva
getClass :: String -> Program -> ClassState
getClass numeC (Program prog) =
        if (numeClasa (head prog)) == numeC
            then head prog
        else getClass numeC (Program (tail prog))

-- intoarce rezultatul unei functii sau Nothing daca nicio functie din cclasa
-- nu are semnatura respectiva
inferGetFunc :: ClassState -> [String] -> Program -> Maybe String
inferGetFunc cls f prog =
    if frez == Nothing && ((numeClasa cls) /= "Global")
        then inferGetFunc  (getClass (clasaParinte cls) prog) f prog
    else if frez == Nothing && ((numeClasa cls) == "Global")
        then Nothing
    else frez
    where
        frez = inferGetFuncAux (func cls) f

-- functie auxiliara pentru a cauta si pe lantul de mostenire o functie
inferGetFuncAux :: [[String]] -> [String] -> Maybe String
inferGetFuncAux [] infunc = Nothing
inferGetFuncAux func infunc =
    if infunc == ( (head (head func)):(tail(tail (head func))) )
        then Just (head (tail (head func)))
    else inferGetFuncAux (tail func) infunc

-- intoarce functia cu semnatura data ca parametru (daca exista)
evalFunc :: [String] -> String -> String -> Program -> Maybe String
evalFunc [] _ _ _ = Nothing
evalFunc str varC numF (Program prog) =
    if cls == Nothing
        then Nothing
    else inferGetFunc (getClass (strMaybe cls) (Program prog)) (numF:str) (Program prog)
    where
        cls = getVarC varC (getGlobalV (Program prog))

-- functie ajutatoare pentru construirea listei de parametri pentru functie
strMaybe :: Maybe String -> String
strMaybe (Just a) = a
strMaybe Nothing = "Nothing"

-- intoarce numele unei variabile din [Expr]
strVa :: Expr -> String
strVa (Va a) = a

-- functie care verifica daca (head [Expr]) este Va sau FCall 
eqTypeExpr :: Expr -> Expr -> Bool
eqTypeExpr (Va _) (Va _) = True
eqTypeExpr (Va _) (FCall _ _ _) = False
eqTypeExpr (FCall _ _ _) (Va _) = False
eqTypeExpr (FCall _ _ _) (FCall _ _ _) = True

{--===================== FUNCTII AUXILIARE INTERPRET =======================--}

--initEmptyProgram va intoare un program care contine doar clasa Global
initGlobalClass :: ClassState
initGlobalClass = ClassState "Global" "Global" [] []

--parse face parsarea partial, am nevoie pentru newvar cand nu exista spatii
sizeString :: [a] -> Integer
sizeString [] = 0
sizeString (x:xs) = 1 + (sizeString xs) 

{-
    pentru acesta parte a aparut wordsBy3, functia face parsarea in functie
de mai multi delimitatori; folosesc functia foldl pentru a verifica daca toti
parametri si tipul resturnat sunt valizi (construiesc o lista doar cu aceste 
elemente, aplic un map pentru a obtine o lista cu True si False in functie de
apartenenta fiecarui element si dupa, folosind functia foldl stabilec daca 
introduc noua functie)
    dupa ce am facut parsarea folosind functia wordBy3 si dupa ce am verificat
tipurile, inserez noua functie in clasa specificata
-}
addFunc :: [String] -> [ClassState] -> [ClassState]
addFunc (t:c:s:ps) prog =
    if (foldl (&&) True (map (tipFunc prog) (t:c:ps) ))
        then insertFunc prog c (s:t:ps)
    else prog

tipFunc :: [ClassState] -> String -> Bool
tipFunc [] str = False
tipFunc l str =
    if str == (numeClasa (head l))
        then True
    else tipFunc (tail l) str

insertFunc :: [ClassState] -> String -> [String] -> [ClassState]
insertFunc prog numeC func =
    if numeC == (numeClasa (head prog))
        then ((insertIntoClass (head prog) Func func):(tail prog))
    else ((head prog):(insertFunc (tail prog) numeC func))

{-
    newClass adauga o clasa noua in program prin concatenarea acesteia;
verific daca clasa exista deja in program (daca exista, intorc programul
fara nicio modificare), iar dupa daca exista clasa parinte (clasa are parinte
clasa "Global" in caz afirmativ) si dupa introduc noua clasa
-}
newClass :: String -> String -> Program -> Program
newClass cls p_cls (Program prog) =
    if classExist cls (Program prog)
        then Program prog
    else if classExist p_cls (Program prog)
        then Program ((newClassState cls p_cls):prog) 
    else Program ((newClassState cls "Global"):prog) 
    

classExist :: String -> Program -> Bool
classExist cls (Program []) = False
classExist cls (Program prog) =
    if (numeClasa (head prog)) == cls
        then True
    else (classExist cls (Program (tail prog)))

{-
    addVar2 a aparut cand am observat ca nu exista mereu spatii intre numele
variabilei si tipul acesteia; stiind ca pot fi doar 3 cazuri in care functia
words nu sparge variabila intr-o lista cu 4 elemente, le-am acoperit cu acesta
functie

    addVar primeste numele variabilei, tipul acesteia si lista de clase, iar
atunci cand gasesc clasa Global introduc noua variabila, dar doar dupa ce,
apeland functia tipVar, verific daca noua variabila are tip valid

    tipVar parcurge fiecare clasa din program si verifica daca tipul variabilei
este valid
-}
addVar :: String -> String -> [ClassState] -> [ClassState]
addVar newvar cls prog =
    if (numeClasa (head prog)) == "Global"
        then ( (ClassState (numeClasa hp) (clasaParinte hp)
            ((newvar:cls:[]):(var hp)) (func hp) ):tp)
    else ( hp:( addVar newvar cls tp) )
    where
        hp = (head prog)
        tp = (tail prog)

addVar2 :: [String] -> [ClassState] -> [ClassState]
addVar2 ls  prog =
    if (sizeString ls) == 2
        then (addVar (caz1!!0) (caz1!!1) prog)
    else (addVar (caz2!!0) (caz2'!!0) prog)
    where
        caz1 = wordsBy2 '=' (head (tail ls))
        caz2 = wordsBy2 '=' (head (tail ls))
        caz2' = wordsBy2 '=' (head (tail (tail ls)))

tipVar :: String -> Program -> Bool
tipVar tip (Program []) = False
tipVar tip (Program prog) =
    if tip == (numeClasa (head prog))
        then True
    else (tipVar tip (Program (tail prog)))

{-
    Functiile de mai jos sunt folosite pentru parsare (3 functii propriu-zise).

wordsBy alaturi de noNowLine primeste stringul-ul din fisier si construieste
lista de instructiuni, fiecare linie valida fiind o instructiune.

wordsBy2 separa un String dupa un delimitator dat ca parametru, iar noChar2 il
elimina din noua lista de Stringuri construita.

wordsBy3 separa un String dat dupa niste delimitatori dati ca parametru intr-un
String.
existDel e folosita pentru a verifica daca intr-un String apare un delimitator 

-}
noNewLine :: [String] -> [String]
noNewLine [] = []
noNewLine (l:ls) =
    if ( l == "\n")
        then (noNewLine ls)
    else if ((head l) == '\n')
        then ((tail l):(noNewLine ls))
    else (l:(noNewLine ls))

wordsBy :: String -> [String]
wordsBy str = noNewLine ( List.groupBy (\x y -> y /= '\n') str )


noChar2 :: Char -> [String] -> [String]
noChar2 char [] = []
noChar2 char (l:ls) =
    if (l == (char:[]) )
        then (noChar2 char ls)
    else if ((head l) == char)
        then ((tail l):(noChar2 char ls))
    else (l:(noChar2 char ls))

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

existDel :: Char -> String -> Bool
existDel char [] = True
existDel char del =
    if char == (head del)
        then False
    else existDel char (tail del)

wordsBy3 :: String -> String -> [String]
wordsBy3 del str = List.groupBy (\x y -> (existDel y del) ) str