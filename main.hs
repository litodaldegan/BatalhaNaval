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
{-This project imports-}
import Generic
import Interaction

{-Representação do jogo. Dois tabuleiros e uma lista de tiros.-}
data BattleShip = 
	BattleShip
	(UBoard (Int,Int) Char)
	(PBoard (Int,Int) Char)
	PcShots
	deriving (Show)

{-Tabuleiro dos navios do usuário-}
type UBoard  = Map
{-Tabuleiro do pc e tiros do usuário-}
type PBoard  = Map
{-Lista de tuplas, para o pc salvar os tiros
  e utilizar de estratégias-}
type PcShots = [(Int,Int)] 


{-Pega as linhas correpondentes nos dados de
  configuração e separa em palavras para a leitura-}
readS :: String -> Int -> Map (Int,Int) Char
readS s n = readBoard (words ((lines s) !! n))


{- --------------------------------------------
   Funções de estratégia de tiro do jogo
   ---------------------------------------------}

{-Gera uma tupla aleatória que representa um tiro-}
randomShot :: IO (Int, Int)
randomShot = do
	i <- myRandom
	j <- myRandom
	return (i,j)

{-Atira em regiões adjacentes de um tiro certo-}
targetShot :: BattleShip -> (Int,Int) -> IO (Int,Int)
targetShot bs@(BattleShip u p s) (i,j) = do
	{-Testa direita-}
	if (checkShot u (i,j+1))
		then return (i,j+1)
		{-Testa baixo-}
		else if (checkShot u (i+1,j))
			then return (i+1,j)
			{-Testa esquerda-}
			else if (checkShot u (i,j-1))
				then return (i,j-1)
				else if (checkShot u (i-1,j))
					{-Testa em cima-}
					then return (i-1,j)
					else return (0,0)

{-Estratégia de tiro Hunt-Target. Enquanto não acerta um
  navio funciona no modo Hunt, tiros aleatórios. Assim
  que acerta um navio, passa para o modo Target-}
targetStrategy :: BattleShip -> IO (Int,Int)
targetStrategy bs@(BattleShip u p s)
	| null s    = parityShotH
	| otherwise = targetShot bs (head s)

{-Estratégia que não atira em blocos vizinhos na horizontal-}
parityShotH :: IO (Int, Int)
parityShotH = do
	i <- myRandom
	j1 <- evenRandom
	j2 <- oddRandom
	if (odd i)
		then return (i,j1)
		else return (i,j2)


{- --------------------------------------------
   Função de controle do jogo
   ---------------------------------------------}

{-Insere todas as posições lidas na Board-}
readBoard :: [String] -> Map (Int,Int) Char
readBoard   []   = Map.empty
readBoard (s:ss) = Map.insert (readTuple s) ship (readBoard ss)

{-Cria um novo tabuleiro vazio-}
newBoard :: IO BattleShip
newBoard = return (BattleShip Map.empty Map.empty [])

{-Inicializa os tabuleiros de jogo-}
initBoard :: IO BattleShip
initBoard = do
	cont <- readFile "conf"
	return (BattleShip (readS cont 0) (readS cont 1) [])

{-Retorna o tabuleiro de navios do usuário-}
userBoard :: BattleShip -> Map (Int,Int) Char
userBoard (BattleShip u p s) = u

{-Retorna o tabuleiro de tiros do usuário -}
pcBoard :: BattleShip -> Map (Int,Int) Char
pcBoard (BattleShip u p s) = p

{-Marca tiro do usuário no tabuleiro-}
userShot :: BattleShip -> (Int,Int) -> IO BattleShip
userShot (BattleShip u p s) indice = do
	if (Map.notMember indice p)
		then return (BattleShip u (Map.insert indice wShot p) s)
		else return (BattleShip u (Map.insert indice sShot p) s)

{-Marca tiro do computador no tabuleiro-}
pcShot :: BattleShip -> IO BattleShip
pcShot bs@(BattleShip u p s) = do
	shot <- targetStrategy  bs

	{-Verificando se o tiro é inválido-}
	if (not $ checkShot u shot)
		then pcShot (BattleShip u p (myTail s))
		else if (Map.notMember shot u)
		then return (BattleShip (Map.insert shot wShot u) p s)
		else return (BattleShip (Map.insert shot sShot u) p (shot:s))

{-Exibe os tabuleiros do usuário e do computador,
  ambos na visão do usuário-}
showBoard :: BattleShip -> IO()
showBoard (BattleShip u p s) = do
	putStrLn "\n       Seu Tabuleiro          Tabuleiro do Inimigo"
	putStrLn "    0 1 2 3 4 5 6 7 8 9       0 1 2 3 4 5 6 7 8 9"
	putStrLn "  |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|  |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|"
	putStr (printBoth (showB u (1,0)) (showBP p (1,0)) 0)
	putStrLn "  |______________________|  |______________________|"
	putStrLn "\n"

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
getShot bs@(BattleShip u p s) = do
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

{-Parte interativa que realiza as rodadas do jogo-}
playBS :: BattleShip -> IO()
playBS bs@(BattleShip u p s) = do
	if (checkEnd p (0,0) True)
		then do
			cleanScreen
			showBoard bs
			showVictory
			return ()
		else if (checkEnd u (0,0) True)
			then do
				cleanScreen
				showBoard bs
				showDefeat
				return ()
		else do
			{-Limpando a tela-}
			cleanScreen
			{-Mostrando os tabuleiro-}
			showBoard bs
			{-Realizando tiro do usuário-}
			bs2 <- getShot bs
			{-Realizando tiro do computador-}
			bs3 <- pcShot bs2
			playBS bs3

checkPosition :: Map (Int,Int) Char -> (Int,Int) -> Int -> Char -> Bool
checkPosition _ _ 0 _ = True
checkPosition m t@(i,j) n o = do
	if (i < 1 || i > 10 || j < 1 || j > 10)
		then False
		else if (Map.notMember t m)
			then if (o == 'v')
				then True && checkPosition m (i+1,j) (n-1) o
			 	else True && checkPosition m (i,j+1) (n-1) o
			else False

{-Função interativa para possicionar os navio no início do jogo-}
positionShips :: Int -> IO ()
positionShips n = do
	bs <- newBoard
	cleanScreen
	putStrLn "Posicione os seus navios no tabueiro de forma"
	putStrLn "que o inimigo não seja capaz de acertá-los."
	showBoard bs
	putStrLn $ "Possicionar " ++ (shipName n)
	putStrLn "Colocar o návio na (v)ertical ou (h)orizontal"
	putStr "Orientação:"
	o <- getOpt "vh"
	putStrLn "Seleciona as coordenadas para o navio:"
	putStr "(Início) Eixo x:"
	x <- getOpt "1 2 3 4 5 6 7 8 9 0"
	putStr "(Início) Eixo y:"
	y <- getOpt "1 2 3 4 5 6 7 8 9 0"

	if (checkPosition (userBoard bs) (digitToInt x,digitToInt y) (shipSize n) o)
		then putStrLn "Navio adicionado com sucesso."
		else putStrLn "Erro"

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
			showEnd
			putStrLn "Obrigado por jogar."
			return ()
