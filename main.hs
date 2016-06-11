module Main where

import System.IO
import Control.Monad
import Data.Char
import Data.List
import System.Random
import Data.Map (Map, (!))
import qualified Data.Map as Map

data BattleShip = BattleShip (UBoard (Int,Int) Char) (PBoard (Int,Int) Char)
	deriving (Show)

type UBoard = Map
type PBoard = Map

{-Lê uma string para um tupla de Int-}
readTuple :: String -> (Int,Int)
readTuple s = read s::(Int,Int)

{-Insere todas as posições lidas na Board-}
readBoard :: [String] -> Map (Int,Int) Char
readBoard   []   = Map.empty
readBoard (s:ss) = Map.insert (readTuple s) '⊡' (readBoard ss)

{-Pega as linhas correpondentes nos dados de
  configuração e separa em palavras para a leitura-}
readS :: String -> Int -> Map (Int,Int) Char
readS s n = readBoard (words ((lines s) !! n))

{-Inicializa os tabuleiros de jogo-}
initBoard :: IO BattleShip
initBoard = do
	cont <- readFile "conf"
	return (BattleShip (readS cont 0) (readS cont 1))

{-Retorna o tabuleiro de navios do usuário-}
userBoard :: BattleShip -> Map (Int,Int) Char
userBoard (BattleShip u p) = u

{-Retorna o tabuleiro de tiros do usuário -}
pcBoard :: BattleShip -> Map (Int,Int) Char
pcBoard (BattleShip u p) = p

{-String de número em 2 caracteres-}
show2Dig :: Int -> String
show2Dig n
	| n < 10    = " " ++ (show n)
	| otherwise = show n

{-Retorna a string com a visão do usuário de seu
  tabuleiro-}
showB :: Map (Int,Int) Char -> (Int,Int) -> String
showB m t@(i,j)
	{-Imprimindo as bordas esquerdas-}
	| j == 0    = if (i < 11)
					then 
						(show2Dig i) ++ "| " ++ (showB m (i,j+1))
						else
							""
	{-Imprimindo as bodas direitas-}
	| j == 11   = if (i > 10)
					then " |\n"
					else (" |\n" ++ showB m (i+1,0))
	| otherwise = if (Map.notMember t m)
					then ("  " ++ showB m (i,j+1))
					else ((m ! (i,j):" ") ++ showB m (i,j+1))

{-Retorna a string com a visão do usuário do
  tabuleiro do adversário-}
showBP :: Map (Int,Int) Char -> (Int,Int) -> String
showBP m t@(i,j)
	{-Imprimindo as bordas esquerdas-}
	| j == 0    = if (i < 11)
					then 
						(show2Dig i) ++ "| " ++ (showBP m (i,j+1))
						else
							""
	{-Imprimindo as bodas direitas-}
	| j == 11   = if (i > 10)
					then " |\n"
					else (" |\n" ++ showBP m (i+1,0))
	{-Imprimindo os tiros-}
	| otherwise = if (Map.notMember t m)
					then ("  " ++ showBP m (i,j+1))
					else do
						if (m ! (i,j) == '⊡')
						 then ("  " ++ showBP m (i,j+1))
						 else ("* " ++ showBP m (i,j+1))

{-Exibe os tabuleiros do usuário e do computador,
  ambos na visão do usuário-}
showBoard :: IO()
showBoard = do
	u <- initBoard
	putStrLn "\n       Seu Tabuleiro"
	putStrLn "    1 2 3 4 5 6 7 8 9 10"
	putStrLn "  |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|"
	putStr (showB (userBoard(u)) (1,0))
	putStrLn "  |______________________|"
	putStrLn "\n    Tabuleiro do Inimigo"
	putStrLn "    1 2 3 4 5 6 7 8 9 10"
	putStrLn "  |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|"
	putStr (showBP (pcBoard(u)) (1,0))
	putStrLn "  |______________________|"


main = do
    let m0 = Map.empty
    let m1 = Map.insert (1,1) " " m0
    let m  = Map.insert (2,2) "*" m1
    putStrLn $ "map: " ++ (show (m ! (1,1)))


--readBoard :: String -> Board
--readBoard 

--initBoard :: String -> Board 
--initBoard []         = (Board (0,0,False)
--initBoard (loc:file) = Board (readBoard file) 