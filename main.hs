module Main where

{-Data import-}
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map
{-This project imports-}
import Generic
import Interaction

{-Representação do jogo. Dois tabuleiros e uma lista de tiros.-}
data BattleShip = 
	BattleShip
	(UBoard)
	(PBoard)
	PcShots
	deriving (Show)

{-Tabuleiro dos navios do usuário-}
type UBoard  = Map (Int,Int) Char
{-Tabuleiro do pc e tiros do usuário-}
type PBoard  = Map (Int,Int) Char
{-Lista de tuplas, para o pc salvar os tiros
  e utilizar de estratégias-}
type PcShots = [(Int,Int)] 

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

{-Estratégia que não atira em blocos vizinhos na horizontal
  e nem na vertical. Variando o bloco atirado para cada linha-}
parityShotH :: IO (Int, Int)
parityShotH = do
	i <- myRandom
	j1 <- evenRandom
	j2 <- oddRandom

	if (odd i)
		then return (i,j1)
		else return (i,j2)

{- --------------------------------------------
   Funções de controle do jogo
   ---------------------------------------------}

{-Pega as linhas correpondentes nos dados de
  configuração e separa em palavras para a leitura-}
readS :: String -> Int -> Map (Int,Int) Char
readS s n = readBoard (words ((lines s) !! n))

{-Insere todas as posições lidas na Board-}
readBoard :: [String] -> Map (Int,Int) Char
readBoard   []   = Map.empty
readBoard (s:ss) = Map.insert (readTuple s) ship (readBoard ss)

{-Cria um novo tabuleiro vazio-}
newBoard :: IO BattleShip
newBoard = return (BattleShip Map.empty Map.empty [])

{-Inicializa os tabuleiros de jogo-}
initBoard :: Bool -> IO BattleShip
initBoard rand = do
	cont <- readFile shipAllocationFile
	pb <- myRandom' (length (lines cont)-1)  
	ub <- myRandom' (length (lines cont)-1)  
	
	{-Se o usuário escolheu usar uma distribuição de navios aleatórias-}
	if (rand)
		{-Escolhe do arquivo de configurações uma distribuição-}
		then return (BattleShip (readS cont ub) (readS cont pb) [])
		{-Pega a distribuição que o usuário acabou de fazer (última entrada)-}
	 	else return (BattleShip (readS cont (length (lines cont)-1)) (readS cont pb) [])

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

	{-Verificando se o tiro é válido-}
	if (not $ checkShot u shot)
		{-Tenta um novo tiro com o penúltimo tiro certo,
		  o último é descartado-}
		then pcShot (BattleShip u p (myTail s))
		{-Verifica se acertou um navio-}
		else if (Map.notMember shot u)
		{-Se não acertou, só marca o tiro no tabuleiro-}
		then return (BattleShip (Map.insert shot wShot u) p s)
		{-Se acertou, salva o tiro-}
		else return (BattleShip (Map.insert shot sShot u) p (shot:s))

{-Exibe os tabuleiros do usuário e do computador,
  ambos na visão do usuário-}
showBoard :: BattleShip -> IO()
showBoard (BattleShip u p s) = do
	putStrLn "\n ┌────── Seu Tabuleiro ─────┐ ┌── Tabuleiro do Inimigo ──┐"
	putStrLn " │     ０１２３４５６７８９ │ │     ０１２３４５６７８９ │"
	putStrLn " ├───┬──────────────────────┤ ├───┬──────────────────────┤"
	putStr (printBoth (showB u (1,0)) (showBP p (1,0)) 0)
	putStrLn " └───┴──────────────────────┘ └───┴──────────────────────┘"
	putStrLn "\n"

{-Verifica se o tabuleiro está com todos navios afundados-}
checkEnd :: Map (Int,Int) Char -> (Int,Int) -> Bool -> Bool
checkEnd m t@(i,j) end
	{-Fim da linha-}
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

	{-Verifica se o tiro é válido-}
	if (checkShot p (getShotOpt x y))
		then (userShot bs (getShotOpt x y))
		else do
			putStrLn " Você já atirou nesse alvo. Selecione outro."
			getShot bs

{-Parte interativa que realiza as rodadas do jogo-}
playBS :: BattleShip -> IO()
playBS bs@(BattleShip u p s) = do
	{-Verifica se o usuário venceu o jogo-}
	if (checkEnd p (0,0) True)
		then do
			cleanScreen
			showBoard bs
			showVictory
			return ()
		{-Verifica se o computador venceu o jogo-}
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


{- --------------------------------------------
   Funções para possicionar os navios no início
   ---------------------------------------------}

{-Verifica se não há navios nas posições para inserir um novo navio-}
checkPosition :: Map (Int,Int) Char -> (Int,Int) -> Int -> Char -> Bool
checkPosition _ _ 0 _ = True
checkPosition m t@(i,j) n o = do
	{-Verifica se as coordenadas são válidas-}
	if (checkCoord t n o)
		then if (Map.notMember t m)
			{-Se a orientação do navio for vertical-}
			then if (o == vertical)
				{-Verifica as posições abaixo da inicial-}
				then True && checkPosition m (i+1,j) (n-1) o
				{-Verifica as posições a lado da inicial-}
			 	else True && checkPosition m (i,j+1) (n-1) o
			else False
		else False

{-Verifica se o navio não vai sair do tabuleiro-}
checkCoord :: (Int,Int) -> Int -> Char -> Bool
checkCoord (i,j) size o = do
	{-Se a orientação do navio for vertical-}
	if (o == vertical)
		then if ((size + i) > 11)
			then False
			else True
		else if ((size + j) > 11)
			then False
			else True

{-Insere o navio no tabuleiro-}
insertShip :: (Int,Int) -> Int -> Char -> UBoard
insertShip t@(x,y)   0  o = Map.empty
insertShip t@(x,y) size o = 
	{-Se a orientação do navio for vertical-}
	if (o == vertical)
		then Map.insert (x,y) ship (insertShip (x+1,y) (size-1) o) 
		else Map.insert (x,y) ship (insertShip (x,y+1) (size-1) o)

{-Função interativa para possicionar navio no tabuleiro-}
positionShips :: UBoard -> Int -> IO (UBoard)
positionShips ub n = do
	putStrLn "    0 1 2 3 4 5 6 7 8 9\n  |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|"
	putStr (showB ub (1,0))
	putStrLn "  |______________________|\n"
	putStrLn $ "Possicionar " ++ (shipName n)
	putStrLn "Colocar o návio na (v)ertical ou (h)orizontal?"
	putStr "Orientação:"
	o <- getOpt "vh"
	putStrLn "Seleciona as coordenadas para o navio:"
	putStr "(Início) Eixo x:"
	x <- getOpt "1 2 3 4 5 6 7 8 9 0"
	putStr "(Início) Eixo y:"
	y <- getOpt "1 2 3 4 5 6 7 8 9 0"
	cord <- coordinate x y

	cleanScreen

	{-Verifica se a posição do navio e válida-}
	if (checkPosition ub cord (shipSize n) o)
		then return (Map.union ub (insertShip cord (shipSize n) o))
		else do
			putStrLn "Posição inválida. Favor selecionar novamente."
			positionShips ub n

{-Função que faz a inserção de todos os navios no tabuleiro-}
position :: IO()
position = do
	cleanScreen
	putStrLn "Posicione os seus navios no tabueiro de forma"
	putStrLn "que o inimigo não seja capaz de acertá-los."

	{-Posicionando os navios Minesweeper-}
	b1 <- positionShips (Map.empty) 0
	b2 <- positionShips b1 0
	b3 <- positionShips b2 0
	b4 <- positionShips b3 0
	--{-Posicionando os navios Frigate-}
	b5 <- positionShips b4 1
	b6 <- positionShips b5 1
	b7 <- positionShips b6 1
	--{-Posicionando os navios Cruiser-}
	b8 <- positionShips b7 2
	b9 <- positionShips b8 2
	--{-Posicionando o BattleShip-}
	b10 <- positionShips b9 3
	
	putStrLn ""

	{-Salvando essa configuração para ser utilizada em
	  outras partias-}
	appendFile shipAllocationFile (giveKeys (Map.keys b10))
	appendFile shipAllocationFile "\n" 

main :: IO()
main = do
	{-Exibe mensagem de início-}
	showEnterDialog
	getChar

	{-Verificando se o jogador quer posicionar os navios-}
	cleanScreen
	putStr "Deseja posicionar os seus navios?"
	putStr " (s - sim / n - não)\nEm caso negativo"
	putStr " será utilizado uma distribuição armazenada.\nOpção:" 
	optP <- getOpt "sn"

	if (optP == 's')
		then do
			{-Posiciona os navios e inicia jogo-}
			position
			bs1 <- (initBoard False)
			playBS bs1
		else do
			{-Inicia com distribuição salva-}
			bs1 <- (initBoard True)
			playBS bs1

	putStr "Deseja jogar novamente? (s - sim / n - não)\nOpção: "
	opt <- (getOpt "sn")
	if (opt == 's')
		then main 
		else do
			showEnd
			putStrLn "Obrigado por jogar."
			return ()
