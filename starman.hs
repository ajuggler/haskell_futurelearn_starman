import System.IO
import System.Random

check :: String -> String -> Char -> (Bool, String)
check word display c =
  (c `elem` word, [if x==c
    then c
    else y | (x,y) <- zip word display])
    
winMsg :: String -> Int -> IO ()
winMsg word n =
  do putStrLn (word ++ "   " ++ take n (repeat '*'))
     putStrLn "Ganaste!"

turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
       then putStrLn "Perdiste"
       else if word==display
              then winMsg word n
              else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStrLn (display ++ "   " ++ take n (repeat '*'))
     putStr "  Adivina una letra: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

starman :: Int -> IO()
starman n = 
  do handle <- openFile "lista_de_palabras.txt" ReadMode
     contents <- hGetContents handle
     let thewords = words contents
     rndm <- randomRIO (0, length thewords - 1)
     let word = thewords !! rndm
     turn word ['_' | x <- word] n

--chooseWord :: IO String
--chooseWord =
--  do handle <- openFile "lista_de_palabras.txt" ReadMode
--     contents <- hGetContents handle
--     let thewords = words contents
--     rndm <- randomRIO (0, length thewords - 1)
--     putStrLn (thewords !! rndm)

