module Print where

import System.IO
import Text.Printf
import SSE (opELL, myLength)

-- stringToDouble(stD)
-- RECEBE UMA LISTA DE STRINGS E RETORNA UMA LISTA DE DOUBLE
stringToDouble :: [String] -> [Double]
stringToDouble xs = [read x :: Double | x<-xs]

-- getPoints(gPts)
-- RECEBE VARIAS LINHAS RETORNA TODOS OS VETORES CONTIDOS EM CADA LINHA
getPoints :: [String] -> [[Double]]
getPoints lines = [stringToDouble (words line) | line <- lines]

-- printGroups(pGps)
-- RECEBE UM ARQUIVO E UMA LISTA DE STRINGS E IMPRIME CADA LISTA EM UMA LINHA DO ARQUIVO, SEQUENCIALMENTE.
printGroups archive groups = pGps archive (toString (opELL (+1) groups))
pGps :: Handle -> [[Char]] -> IO ()
pGps archive [] = do hPutStr archive ""
pGps archive (x:xs)
 | x == "," = do hPutStr archive x
                 hPutStr archive " "
                 pGps archive xs
 | otherwise = do hPutStr archive x
                  pGps archive xs

-- listIntToString(lITS)
-- RECEBE UMA LISTA DE INTEIROS E RETORNA UMA LISTA DE STRINGS
listIntToString :: [Int] -> [String]
listIntToString [] = []
listIntToString (x:xs)
 | myLength (x:xs) == 1 = (show x):[]
 | otherwise = ((show x) ++ ", ") : (listIntToString xs)

-- toString(tS)
-- RECEBE UMA LISTA DE LISTA DE INTEIROS E RETORNA UMA LISTA DE STRINGS
toString :: [[Int]] -> [[Char]]
toString [] = ["\n\n"]
toString [x] = (listIntToString x)++["\n\n"]
toString groups = (listIntToString (head groups)) ++ ["\n\n"] ++ (toString (tail groups))

-- printSSE(sSSE)
-- RECEBE UM NÚMERO E UM ARQUIVO E IMPRIME NO ARQUIVO O NÚMERO COM 4 CASAS DECIMAIS
printSSE archive sse = hPutStr archive (printf "%.4f" sse)