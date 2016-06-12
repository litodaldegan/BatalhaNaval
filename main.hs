module Main where

{-Control import-}
import Control.Monad
{-System import-}
import System.IO
import System.Random
{-Data import-}
import Data.Char
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

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

{-Representação do jogo. Dois tabuleiros-}
data BattleShip = 
	BattleShip
	(UBoard (Int,Int) Char)
	(PBoard (Int,Int) Char)
	deriving (Show)

{-Tabuleiro dos navios do usuário-}
type UBoard = Map
{-Tabuleiro do pc e tiros do usuário-}
type PBoard = Map

{-Lê uma string para um tupla de Int-}
readTuple :: String -> (Int,Int)
readTuple s = read s::(Int,Int)

{-Gera um número aleatório até 10-}
myRandom :: IO Int
myRandom = randomRIO (fromInteger(1),fromInteger(10))

{-Gera um número aleatório par até 10-}
evenRandom :: IO Int
evenRandom = do
	x <- myRandom
	if ((mod x 2) == 0)
		then return x
		else evenRandom

{-Gera uma tupla aleatória que representa um tiro-}
randomShot :: IO (Int, Int)
randomShot = do
	i <- myRandom
	j <- myRandom
	return (i,j)

{-Estratégia que não atira em blocos vizinhos na horizontal-}
parityShotH :: IO (Int, Int)
parityShotH = do
	i <- myRandom
	j <- evenRandom
	return (i,j)

{-Insere todas as posições lidas na Board-}
readBoard :: [String] -> Map (Int,Int) Char
readBoard   []   = Map.empty
readBoard (s:ss) = Map.insert (readTuple s) ship (readBoard ss)

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

{-Marca tiro do usuário no tabuleiro-}
userShot :: BattleShip -> (Int,Int) -> IO BattleShip
userShot (BattleShip u p) indice = do
	if (Map.notMember indice p)
		then return (BattleShip u (Map.insert indice wShot p))
		else return (BattleShip u (Map.insert indice sShot p))

sf :: (Int, Int) -> String
sf (x,y) = (show (x-1) ++ " " ++ show (y-1))

{-Marca tiro do computador no tabuleiro-}
pcShot :: BattleShip -> IO BattleShip
pcShot bs@(BattleShip u p) = do
	
	{-Modo Hunt (caça) -}
	{-Estratégia de tiro-}
	shot <- parityShotH
	--shot <- randomShot
	
	putStrLn (sf shot)

	{-Verificando se não é repetido-}
	if (not $ checkShot u shot)
		then pcShot bs
		else if (Map.notMember shot u)
		then return (BattleShip (Map.insert shot wShot u) p)
		else return (BattleShip (Map.insert shot sShot u) p)

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
						(show2Dig (i-1)) ++ "| " ++ (showB m (i,j+1))
						else
							""
	{-Imprimindo as bodas direitas-}
	| j == 11   = if (i > 10)
					then " |\n"
					else (" |\n" ++ showB m (i+1,0))
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
						(show2Dig (i-1)) ++ "| " ++ (showBP m (i,j+1))
						else
							""
	{-Imprimindo as bodas direitas-}
	| j == 11   = if (i > 10)
					then " |\n"
					else (" |\n" ++ showBP m (i+1,0))
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

{-Imprime as linhas de dois tabuleiros simultaneamente-}
printBoth :: String -> String -> Int -> String
printBoth _  _ 10 = [] 
printBoth [] [] n = []
printBoth s1 s2 n =
	((lines s1) !! n) ++ ((lines s2) !! n) ++ "\n"
		++ printBoth s1 s2 (n+1) 

{-Exibe os tabuleiros do usuário e do computador,
  ambos na visão do usuário-}
showBoard :: BattleShip -> IO()
showBoard (BattleShip u p) = do
	putStrLn "\n       Seu Tabuleiro          Tabuleiro do Inimigo"
	putStrLn "    0 1 2 3 4 5 6 7 8 9       0 1 2 3 4 5 6 7 8 9"
	putStrLn "  |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|  |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|"
	putStr (printBoth (showB u (1,0)) (showBP p (1,0)) 0)
	putStrLn "  |______________________|  |______________________|"
	putStrLn "\n"

{-Exibe mensagem inicial do jogo-}
showEnterDialog :: IO ()
showEnterDialog = do
	cont <- readFile ".entermsg"
	putStrLn cont

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

{-Verifica se o tabuleiro está com todos navios afundados-}
checkEnd :: Map (Int,Int) Char -> (Int,Int) -> Bool -> Bool
checkEnd m t@(i,j) end
	| j == 11   = if (i > 10)
					then end
					else (end && (checkEnd m (i+1,0) True))
	| otherwise = if (Map.member t m)
					then if (m ! (i,j) == ship)
						then False
				    	else (end && checkEnd m (i,j+1) True)
				    else (end && checkEnd m (i,j+1) True)

{-Pega os tiros e retorna uma tupla-}
getShotOpt :: Char -> Char -> (Int,Int)
getShotOpt x y = (digitToInt x + 1,digitToInt y + 1) 

{-Pega um tiro do usuário-}
getShot :: BattleShip -> IO BattleShip
getShot bs@(BattleShip u p) = do
	putStrLn "Seleciona as coordenadas do seu próximo míssel."
	putStr "Eixo x:"
	x <- getOpt "1 2 3 4 5 6 7 8 9 0"
	putStr "Eixo y:"
	y <- getOpt "1 2 3 4 5 6 7 8 9 0"

	if (checkShot p (getShotOpt x y))
		then (userShot bs (getShotOpt x y))
		else do
			putStrLn " Você já atirou nesse alvo. Selecione outro."
			getShot bs
{-
hunt
	tiro acertado
target
	testa direita
		se sucesso target deste
	testa baixo
		se sucesso target deste
	testa esquerda
		se sucesso target deste
	testa acima
		se sucesso target deste
-}

{-Estratégia que tentar exterminar navio que já descoberto-}
targetShot :: BattleShip -> (Int,Int) -> IO ((Int,Int))
targetShot bs@(BattleShip u p) (i,j) = do
	{-Testa direita-}
	if (not $ checkShot u (i,j+i))
		then return (i,j+i)
		{-Testa baixo-}
		else if (not $ checkShot u (i+1,j))
			then return (i+1,j)
			{-Testa esquerda-}
			else if (not $ checkShot u (i,j-i))
				then return (i,j-1)
				else if (not $ checkShot u (i-1,j))
					then return (i-1,j)
					else return (0,0)

{-Parte interativa que realiza as rodadas do jogo-}
playBS :: BattleShip -> IO()
playBS bs@(BattleShip u p) = do
	if (checkEnd p (0,0) True)
		then do
			putStrLn "Você venceu"
			return ()
		else if (checkEnd u (0,0) True)
			then do
				putStrLn "Você perdeu"
				return ()
		else do
			{-Limpando a tela-}
			--putStr "\ESC[2J"
			{-Mostrando os tabuleiro-}
			showBoard bs
			{-Realizando tiro do usuário-}
			bs2 <- getShot bs
			{-Realizando tiro do computador-}
			bs3 <- pcShot bs2
			playBS bs3

main :: IO()
main = do
	showEnterDialog
	getChar

	bs <- initBoard
	playBS bs

	putStrLn "Deseja jogar novamente? (s - sim / n - não)"
	putStr "Opção: "
	opt <- (getOpt "sn")
	if (opt == 's')
		then main 
		else do
			putStrLn "Obrigado por jogar."
			return ()
