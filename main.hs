import System.IO
import SSE
import Print


main = do -- ABRIR ARQUIVOS
          inputPoints <- openFile "entrada.txt" ReadMode
          inputK <- openFile "k.txt" ReadMode
          outputSSE <- openFile "result.txt" WriteMode
          outputGROUPS <- openFile "saida.txt" WriteMode

          -- LER ARQUIVOS
          number <- hGetLine inputK
          fullContent <- hGetContents inputPoints

          -- CÁLCULOS
          let k = (read number) :: Int
          let inputLines = lines fullContent
          let points = getPoints inputLines
          let groups = defGPS points k
          let sse = calcSSE points groups 0
          
          -- IMPRESSÃO
          printGroups outputGROUPS (groups)
          printSSE outputSSE sse 

          -- FECHAR ARQUIVOS
          hClose inputPoints
          hClose inputK
          hClose outputSSE
          hClose outputGROUPS
