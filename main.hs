{-
	Batalha Naval:

	1 Etapa:
	Desenvolver o jogo com o computador jogando
	aleatóriamente e a configurações sendo passadas
	por um arquivo txt.

	2 Etapa:
	Criar uma função que interage com o usuário para
	definir a configuração inicial do jogo.

	3 Etapa:
	Desenvolver uma estratégia de jogadas para o
	PC, onde a jogada é baseada nos último tiros
	realizados.

-}
module Main where

import System.IO
import Control.Monad
import Data.Char
import Data.List
import Data.Char
import System.Random
import Array

data BS = BS Board
	deriving (Show)

type Board = Array (Int, Int) Char

initBoard :: String -> Forca
initBoard [] = Board (0,0) [(,)]
initBoard file = Board (readBoard file) 
