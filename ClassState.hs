{-  
    DUTA VIOREL-IONUT, 321CB
    Tema 1 PP
-}

module ClassState
where

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

-- stocheza toate informatiile aferente unei clase
data ClassState = ClassState { numeClasa :: String
                             , clasaParinte :: String
                             , var :: [[String]]
                             , func :: [[String]]
                             } deriving (Show, Eq)

--totul e initiat cu vid
initEmptyClass :: ClassState
initEmptyClass = ClassState "" "" [] []

--adaug variabila, respectiv functia (in functie de instrType) 
--si intorc ClassState-ul care are restul elementelor neschimbate
insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass classState instrType info =
    if instrType == Var
        then ( ClassState (numeClasa classState) (clasaParinte classState)
        ( info:(var classState) ) (func classState) )
    else ( ClassState (numeClasa classState) (clasaParinte classState) 
    (var classState) ( info:(func classState) ) )

--verific daca trebuie sa intorc variabilele sau functiile
--datorita implementarii pot sa intorc direcr lista
getValues :: ClassState -> InstrType -> [[String]]
getValues classState instrType = 
    if instrType == Var
        then (var classState)
    else (func classState)

--functie folosita in Parser pentru a construi o clasa noua
newClassState :: String -> String -> ClassState
newClassState nume parinte = ClassState nume parinte [] []
