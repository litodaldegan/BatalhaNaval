module Generic where 

import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.Random

{- --------------------------------------------
   Definições para o jogo
   ---------------------------------------------}

{-Caracter que representa partes de navio-}
ship :: Char
ship = '⊡'

{-Caracter que representa água-}
water :: Char
water = '〜'

{-Tiro na água-}
wShot :: Char
wShot = '⊚'

{-Tiro em navio-}
sShot :: Char
sShot = '☠'

shipName :: Int -> String
shipName 0 = "Battleship (tamanho: 5)."
shipName 1 = "Cruiser (tamanho: 4)."
shipName 2 = "Frigate (tamanho: 3)."
shipName 3 = "Minesweeper (tamanho: 2)."

shipSize :: Int -> Int
shipSize 0 = 5
shipSize 1 = 4
shipSize 2 = 3
shipSize 3 = 2

{- --------------------------------------------
   Funções genéricas de manipulação
   ---------------------------------------------}

{-Lê uma string para um tupla de Int-}
readTuple :: String -> (Int,Int)
readTuple s = read s::(Int,Int)

{-Tail seguro, funciona para string vazia-}
myTail :: [a] -> [a]
myTail s
	| null s    = []
	| otherwise = tail s

{-String de número em 2 caracteres-}
show2Dig :: Int -> String
show2Dig n
	| n < 10    = " " ++ (show n)
	| otherwise = show n

{-Imprime as linhas de duas strings simultaneamente-}
printBoth :: String -> String -> Int -> String
printBoth _  _ 10 = [] 
printBoth [] [] n = []
printBoth s1 s2 n =
	((lines s1) !! n) ++ ((lines s2) !! n) ++ "\n"
		++ printBoth s1 s2 (n+1)

{- --------------------------------------------
   Funções de número aleatório
   ---------------------------------------------}

{-Gera um número aleatório até 10-}
myRandom :: IO Int
myRandom = randomRIO (fromInteger(1),fromInteger(10))

{-Gera um número aleatório par até 10-}
evenRandom :: IO Int
evenRandom = do
	x <- myRandom
	if (even x)
		then return x
		else evenRandom

{-Gera um número aleatório ímpar até 10-}
oddRandom :: IO Int
oddRandom = do
	x <- myRandom
	if (odd x)
		then return x
		else oddRandom