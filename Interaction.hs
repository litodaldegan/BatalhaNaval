module Interaction where

import Generic
import Data.Map (Map, (!))
import qualified Data.Map as Map

{- --------------------------------------------
   Funções de interação com terminal
   ---------------------------------------------}

{-Comando para dar scape no terminal-}
cleanScreen :: IO()
cleanScreen = putStr "\ESC[2J"

{-Imprime as strings de uma lista de string-}
showLines :: [String] -> IO()
showLines [] = return ()
showLines s = do 
	putStrLn (head s)
	showLines (tail s)

{-Exibe mensagem inicial do jogo-}
showEnterDialog :: IO ()
showEnterDialog = do
	cont <- readFile ".entermsg"
	showLines (take 35 (lines cont))

{-Mensagem de vitória-}
showDefeat :: IO()
showDefeat = do
	cont <- readFile ".entermsg"
	showLines (take 7 (drop 35 (lines cont)))

{-Mensagem de derrota-}
showVictory :: IO()
showVictory = do
	cont <- readFile ".entermsg"
	showLines (take 6 (drop 42 (lines cont)))

{-Mensagem de fim-}
showEnd :: IO()
showEnd = do
	cont <- readFile ".entermsg"
	showLines (take 7 (drop 48 (lines cont)))

{- --------------------------------------------
   Funções de interação com usuário
   ---------------------------------------------}

{-Lê uma opção (Char) do teclado e valida-}
getOpt :: String -> IO Char
getOpt s = do
  x <- getChar
  putStrLn ""
  if (elem x s)
    then
      return x
      else do
        putStrLn " Opção inválida. Por favor selecionar novamente."
        getOpt s

{- --------------------------------------------
   Função de interação com o tabuleiro
   ---------------------------------------------}

{-Verifica se o tiro já não foi feito-}
checkShot :: Map (Int,Int) Char -> (Int,Int) -> Bool
checkShot m t@(i,j) = do
	if (i < 1 || i > 10 || j < 1 || j > 10)
		then False
		else if (Map.notMember t m)
			 then True
			 else if (m ! t == ship)
				then True
				else False

{-Retorna a string com a visão do usuário de seu
  tabuleiro-}
showB :: Map (Int,Int) Char -> (Int,Int) -> String
showB m t@(i,j)
	{-Imprimindo as bordas esquerdas-}
	| j == 0    = if (i < 11)
					then 
						(" " ++ show2Dig (i-1)) ++ " │ " ++ (showB m (i,j+1))
						else
							""
	{-Imprimindo as bordas direitas-}
	| j == 11   = if (i > 10)
					then " │\n"
					else (" │\n" ++ showB m (i+1,0))
	| otherwise = if (Map.notMember t m)
					{-Imprimindo água-}
					then (water : showB m (i,j+1))
					{-Imprimindo os barcos do usuário-}
					else ((m ! (i,j):" ") ++ showB m (i,j+1))

{-Retorna a string com a visão do usuário do
  tabuleiro do adversário-}
showBP :: Map (Int,Int) Char -> (Int,Int) -> String
showBP m t@(i,j)
	{-Imprimindo as bordas esquerdas-}
	| j == 0    = if (i < 11)
					then 
						(show2Dig (i-1)) ++ " │ " ++ (showBP m (i,j+1))
						else
							""
	{-Imprimindo as bodas direitas-}
	| j == 11   = if (i > 10)
					then " │\n"
					else (" │\n" ++ showBP m (i+1,0))
	{-Imprimindo os tiros-}
	| otherwise = if (Map.notMember t m)
					{-Imprimindo água-}
					then (water : showBP m (i,j+1))
					else do
						if (m ! (i,j) == ship)
						 {-Escondendo os barcos do computador-}
						 then (water : showBP m (i,j+1))
						 else if (m ! (i,j) == wShot)
						 	{-Tiro realizado na água-}
						 	then (wShot : " " ++ showBP m (i,j+1))
						 	{-Tiro realizado em barco-}
						 	else (sShot : " " ++ showBP m (i,j+1))
