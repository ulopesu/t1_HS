module SSE where

import Numeric


-- myLength(mL)
-- CALCULA O TRAMANHO DE UMA LISTA
-- EX: myLength [9,5,3] -> 3
myLength xs = sum [1 | _ <- xs]

-- operateList(opL)
-- REALIZA UMA OPERAÇÃO MATEMATICA EM TODOS OS ELEMENTOS DE UMA LISTA DE NUMEROS.
-- EX: opL (/5) [5,5,5] -> [1,1,1]
opL :: (Num a) => (a -> a) -> [a] -> [a]
opL f [x] = [f x]
opL f [] = []
opL f (x:xs) = (f x):opL f xs

-- operateElementsOnListsofLists(opELL)
-- REALIZA UMA OPERAÇÃO MATEMATICA EM TODOS OS ELEMENTOS DE UMA DE LISTA LISTA DE NUMEROS.
-- EX: opL (/5) [[5],[5],[5]] -> [[1],[1],[1]]
opELL f [x] = [opL f x]
opELL f [] = []
opELL f (xs:xxs) = (opL f xs):(opELL f xxs)

-- operate2Lists(o2L)
-- REALIZA UMA OPERAÇÃO MATEMATICA ENTRE DUAS LISTA DE NUMEROS.
-- EX: o2L (+) [1,1,1] [1,1,1] -> [2,2,2]
o2L :: (Num a) => (a -> a -> a) -> [a] -> [a] -> [a]
o2L _ [] [] = []
o2L _ xs [] = xs
o2L _ [] xs = xs
o2L  f (x:xs) (y:ys) = (f x y):o2L f xs ys

-- operateBetweenListsofLists(oBLL)
-- REALIZA UMA OPERAÇÃO MATEMATICA ENTRE TODAS AS LISTA DE LISTA DE NUMEROS.
-- EX: oBLL (+) [[1,1,1], [1,1,1], [1,1,1]] -> [3,3,3]
oBLL :: (Num a) => (a -> a -> a) -> [[a]] -> [a]
oBLL f [] = []
oBLL f [[]] = []
oBLL f [[xs]] = [xs]
oBLL f (xs:xxs) = o2L f xs (oBLL f xxs)

-- centroid(c)
-- DADO UMA LISTA DE PONTOS, CALCULA O CENTROIDE DOS MESMOS
-- EX: centroid [[0,0],[4,4]] -> [2]
centroid :: (Num a, Fractional a) => [[a]] -> [a]
centroid xxs =  opL (/(myLength xxs)) (oBLL (+) xxs)

-- distance2Pts(dist2Pts)
-- CÁLCULA A DISTÂNCIA EUCLIDIANA ENTRE DOIS PONTOS MESMOS
-- EX: dist2Pts [0,0,0] [1,2,2] -> [3]
dist2Pts :: (Num a, Fractional a, RealFloat a) => [a] -> [a] -> a
dist2Pts xs ys = (sum (opL (^2) (opL (abs) (o2L (-) xs ys))))**0.5

-- takedrop(tD)
-- DADO UM ÍNDICE(i), UM NÚMERO(n) E UM VETOR(v), APAGA OS n ELEMENTOS DE i ATÉ i+n, NO VETOR v, E RETORNA O RESTANTE.
-- EX: takedrop 1 2 [0,1,2,3] -> [0,3]
takedrop :: Int -> Int -> [b] -> [b]
takedrop n m xs  = take n xs ++ (drop (n+m) xs)

-- select2Point(s2P)
-- DADOS DOIS PONTOS (P1 E P2), VERIFICA COORDENADA A COORDENADA QUAL É MENOR, RETORNA 0(P1 MENOR OU IGUAL) OU 1(P2 MENOR).
-- EX: s2P [0,1,0] [1,0,0] -> 0
s2P :: (Eq a, Ord a) => [a] -> [a] -> Bool
s2P [] [] = True
s2P [] ys = True
s2P xs [] = False
s2P (x:xs) (y:ys)
 | x < y = True
 | x > y = False
 | otherwise = s2P xs ys

-- selectSmallerPoint(sSP)
-- DADO UMA LISTA DE PONTOS, VERICA QUAL É O MAIS PRÓXIMO DA ORIGEM E RETORNA SEU ÍNDICE.
-- EX: selectSmallerPoint [[1,1,2],[2,1,0],[1,1,0]] -> 2
selectSmallerPoint :: (Num a, Ord a) => [[a]] -> Int
selectSmallerPoint xxs = sSP xxs 0 0
sSP :: (Num a, Ord a) => [[a]] -> Int -> Int -> Int
sSP [xs] idS id = idS
sSP (x:xs) idS id
 | sum1 < sum2 = keep
 | sum1 > sum2 = swap
 | s2P x (head xs) = keep
 | otherwise = swap
   where keep = sSP (takedrop 1 1 (x:xs)) idS (id + 1)
         swap = sSP xs (id + 1) (id + 1)
         sum1 = sum x
         sum2 = sum(head xs)

-- selectMoreDistantPoint(sMDP)
-- DADO UMA LISTA DE PONTOS E O INDICE DE UM PONTO, VERICA QUAL DA LISTA É O MAIS DISTÂNTE DO PONTO DADO E RETORNA SEU ÍNDICE.
-- EX: selectMoreDistantPoint [[0,0,0],[4,4,4],[5,5,5],[3,3,3]] 3 -> 0
selectMoreDistantPoint :: (RealFloat a, Ord a) => [[a]] -> Int -> Int
selectMoreDistantPoint xxs idPoint = sMDP xxs 0 0 (xxs!!idPoint)
sMDP :: (RealFloat a, Ord a) => [[a]] -> Int -> Int -> [a] -> Int
sMDP [xs] idS id point = idS
sMDP (x:xs) idS id point
 | dist1 > dist2 = keep
 | dist1 < dist2 = swap
 | s2P x (head xs) = keep
 | otherwise = swap
   where keep = sMDP (takedrop 1 1 (x:xs)) idS (id + 1) point
         swap = sMDP xs (id + 1) (id + 1) point
         dist1 = dist2Pts x point
         dist2 = dist2Pts (head xs) point 

-- selectForCentroid(sFC)
-- DADOS UMA LISTA DE PONTOS E UMA LISTA DE ÍNDICES.
-- RETORNA O ÍNDICE DO PONTO, QUE ESTÁ MAIS LONGE DO CENTROID FORMADO PELOS PONTOS, CUJOS INDICES JÁ ESTÃO NA LISTA.
-- EX: selectForCentroid [[0,0],[1,1],[2,2],[4,4]] [0,3] -> 1
selectForCentroid :: (Num a, Ord a, RealFloat a) => [[a]] -> [Int] -> Int
selectForCentroid xxs listSelect = sFC xxs (centroid (sBIDS xxs listSelect)) listSelect 0 0
sFC :: (Num a, Ord a, RealFloat a) => [[a]] -> [a] -> [Int] -> Int -> Int -> Int
sFC [xs] centroid listSelect idS id = idS
sFC (x:xs) centroid listSelect idS id
 | (isFound listSelect idS) = swap
 | (isFound listSelect (id + 1)) = keep
 | dist1 > dist2 = keep
 | dist1 < dist2 = swap
 | s2P x (head xs) = keep
 | otherwise = swap
   where keep = sFC (takedrop 1 1 (x:xs)) centroid listSelect idS (id + 1)
         swap = sFC xs centroid listSelect (id + 1) (id + 1)
         dist1 = dist2Pts x centroid
         dist2 = dist2Pts (head xs) centroid

-- selectPoins(sP)
-- DADOS UMA LISTA DE PONTOS E UM NÚMERO INTEIRO(k). RETORNA UMA LISTA COM k INDICES, SE A LISTA CONTER K OU MAIS ELEMENTOS.
-- EX: selectPoints [[2,2],[1,1],[0,0]] 2 -> [2,0]
selectPoints :: (Num a, Ord a, RealFloat a) =>  [[a]] -> Int -> [Int]
selectPoints xxs k = sP xxs k []
sP :: (Num a, Ord a, RealFloat a) =>  [[a]] -> Int -> [Int] -> [Int]
sP xxs k selected
 | k == myLength(selected) = selected
 | myLength(selected) == 0 = sP xxs k (iL (selectSmallerPoint xxs) selected)
 | myLength(selected) == 1 = sP xxs k (iL (selectMoreDistantPoint xxs (selected!!0)) selected)
 | otherwise = sP xxs k (iL (selectForCentroid xxs selected) selected)         

-- selectIndex(sID)
-- RECEBE UM PONTO E UMA LISTA DE CENTROIDES E ROTORNA O INDICE DO CENTROID CUJO PONTO ESTÁ MAIS PRÓXIMO
-- EX: sID [1,1,1] [[0,0,0],[3,3,3],[-1,-1,-1]] -> 0
sID :: (Ord a, RealFloat a) => [a] -> [[a]] -> Int
sID xs yys = sI xs yys 0 0
sI :: (Ord a, RealFloat a) => [a] -> [[a]] -> Int -> Int -> Int
sI xs [] idS position = idS
sI xs [ys] idS position = idS
sI xs (ys:yys) idS position
 | dist1 < dist2 = keep
 | dist1 > dist2 = swap
 | s2P ys (head yys) = keep
 | otherwise = swap
   where keep = sI xs (takedrop 1 1 (ys:yys)) idS (position+1)
         swap = sI xs yys (position+1) (position+1)
         dist1 = dist2Pts xs ys
         dist2 = dist2Pts xs (head yys)

-- selectByIndexes(sBIDS)
-- DADO UM VETOR(v) E UMA  LISTA DE ÍNDICES(li) E, RETORNAR TODOS O ELEMENTOS, DO VETOR v, CUJO ÍNDICE ESTÁ NA LISTA.
-- EX: sBIDS [0,1,2] [0,2] -> [0,2]
sBIDS :: [a] -> [Int] -> [a]
sBIDS xs indexes = sBI xs indexes []
sBI :: [a] -> [Int] -> [a] -> [a]
sBI [] indexes selects = selects
sBI xs [] selects = selects
sBI xs indexes selects = sBI xs (tail indexes) (selects++[(xs !! (head indexes))])

-- isFound(iF)
-- VEFIFICAR SE UM VALOR n PERTENCE A LISTA, E RETORNA TRUE OU FALSE.
-- isFound [9,6,58,2,7] 6 -> True
isFound [] n = False
isFound (x:xs) n
 | x == n = True
 | otherwise = isFound xs n
-- insertOnList(iL)
-- INSERE UM NÚMERO NO FINAL DA LISTA
-- EX: iL 1 [0,2,3] -> [0,2,3,1]
iL :: a -> [a] -> [a]
iL x ys = ys++[x]

-- insertOnGroups(iGps)
-- DADOS UMA DE LISTA LISTA DE ÍNDICES DE UM PONTOS, O ÍNDICE DE UM PONTO E O ÍNDICE DA LISTA. INSERE O ÍNDICE DO PONTO NA LISTA DE LISTAS.
-- EX: iGps [[1,3,8],[6,7],[4,9,2]] 5 1 -> [[1,3,8],[5,6,7],[4,9,2]]
iGps :: [[Int]] -> Int -> Int -> [[Int]]
iGps [] x 0 =  [x]:[]
iGps [] x index =  []:(iGps [] x (index-1))
iGps (ys:yys) x 0 =  (iL x ys):yys
iGps (ys:yys) x index = ys:(iGps yys x (index-1))

-- calcGrupos(calcGps)
-- DADOS UMA LISTA DE PONTOS E UMA LISTA DE CENTROIDES DEFINE UMA LISTA DE LISTA DE INDICES DOS PONTOS (GRUPOS)
-- EX: calcG [[0,0,0],[3,3,3],[5,5,5]] [[1,1,1],[4,4,4]] -> [[0],[1,2]]
calcGps :: (Ord a, RealFloat a) => [[a]] -> [[a]] -> [[Int]]
calcGps xxs centroids = calcGpsN xxs centroids [] 0
calcGpsN :: (Ord a, RealFloat a) => [[a]] -> [[a]] -> [[Int]] -> Int -> [[Int]]
calcGpsN [] centroids groups pointID = groups
calcGpsN (xs:xxs) centroids groups pointID = calcGpsN xxs centroids newGroups (pointID+1)
                                             where newGroups = iGps groups pointID (sID xs centroids)

-- defineGroups(defGPS)
-- DADOS UMA LISTA DE PONTOS E UM NUMERO K, CRIA K GRUPOS COM OS PONTOS
-- EX: defGPS [[0,0,0],[2,2,2],[3,3,3]] 2 -> [[0],[1,2]]
defGPS :: (Ord a, RealFloat a) => [[a]] -> Int -> [[Int]]
defGPS xxs k = defGPSN xxs (sBIDS xxs (selectPoints xxs k)) 99
defGPSN :: (Ord a, RealFloat a) => [[a]] -> [[a]] -> Int -> [[Int]]
defGPSN xxs centroids n
 | n == 0 = groups
 | centroids == newCentroids = groups
 | otherwise = defGPSN xxs newCentroids (n-1)
   where groups = calcGps xxs centroids
         newCentroids = ctdsGps xxs groups

-- centroidsGrupos(ctdsGps)
ctdsGps :: (Fractional a) => [[a]] -> [[Int]] -> [[a]]
ctdsGps xxs [] = []
ctdsGps xxs (y:ys) = (centroid (sBIDS xxs y)):ctdsGps xxs ys

-- somatorioDistanciasEuclidianasQuadradas(calcSSE)
-- DADOS UMA LISTA DE PONTOS E UMA LISTA DE LISTA DE INDICES, RETORNA O SOMATORIO DAS DISTÂNCIAS EUCLIDIANAS ENTRE CADA PONTO E O CENTROIDO DO SEU GRUPO.
-- calcSSE [[0,0,0],[1,1,1],[2,2,2]] [[1],[0,2]] 0 -> 5.999999999999999
calcSSE :: (RealFloat a) => [[a]] -> [[Int]] -> a -> a
calcSSE [] [] sse = 0
calcSSE xxs [] sse = sse
calcSSE xxs (gs:ggs) sse = calcSSE xxs ggs (sse + (calcSSEGroup xxs gs (centroid (sBIDS xxs gs)) 0))
calcSSEGroup :: (RealFloat a) => [[a]] -> [Int] -> [a] -> a -> a
calcSSEGroup [] group centroid sse = 0
calcSSEGroup xxs [] centroid sse = sse
calcSSEGroup xxs (g:gs) centroid sse =  calcSSEGroup xxs gs centroid (sse + ((dist2Pts (xxs!!g) centroid)^2))