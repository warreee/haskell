import Commandos
import Control.Monad (liftM)
import Data.List (intercalate)
import System.Random (getStdRandom, randomR)
import Data.List(transpose, elemIndices)

data Direction = Lef | Up | Down | Rig
game :: [Command Board]
game = 	[iofunc "restart" (\x -> do 
					initB <- insertRandom emptyBoard
					putStrLn $ showBoard initB
					return initB)
		,iofunc "l" (move Lef)
		,iofunc "r" (move Rig)
		,iofunc "u" (move Up)
		,iofunc "d" (move Down)
		]

type Cell  = Integer
type Row   = [Cell]
type Board = [Row]

gameSize :: Int
gameSize = 4

-- converts the board to a human-readable form
showBoard :: Board -> String
showBoard = unlines . fmap formatRow
    where formatRow = intercalate "|" . fmap (center 4 ' '. formatCell)
          formatCell 0 = " "
          formatCell x = show x
          center :: Int -> Char -> String -> String
          center k c t
            | len >= k  = t
            | otherwise = replicate l c ++ t ++ replicate r c
              where 
                  len = length t
                  d   = k - len
                  r   = d `quot` 2
                  l   = d - r

-- inserts a random number into the board
insertRandom :: Board -> IO (Board)
insertRandom board
    | null holes = error "No free spaces left: Game Over!"
    | otherwise = do
        pos <- liftM (holes !!) $ getStdRandom $ randomR (0, length holes - 1)
        coin <- getStdRandom $ randomR (0 :: Float, 1)
        let newCell = if coin < 0.9 then 2 else 4
        return $ update board pos newCell
    where 
		holes = available board
		-- | coords of available spaces
		available :: Board -> [(Int, Int)]
		available = concat . zipWith (zip . repeat) [0..] . fmap (elemIndices 0)

		-- | update a specific coordinate in the board
		update :: Board -> (Int, Int) -> Cell -> Board
		update board (x, y) val = newBoard
		    where (rs, r:rs') = splitAt x board
		          (cs, _:cs') = splitAt y r
		          newRow = cs ++ (val : cs')
		          newBoard = rs ++ (newRow : rs')

-- Returns a board filled with zeroes of size gameSize * gameSize
emptyBoard :: Board
emptyBoard = replicate gameSize $ replicate gameSize 0

-- Moves and squashes all numbers to the right
moveRight :: Board -> Board
moveRight = map moveRow
  where 
	moveRow :: Row -> Row
	moveRow = pad . combine . filter (/= 0) 
	
	combine :: Row -> Row
	combine (a:b:xs) |a ==b = (a+b) : combine xs
					 |otherwise = a : combine (b:xs)
	combine x = x

	pad :: Row -> Row
	pad xs = replicate (gameSize - length xs) 0 ++ xs

-- Uses moveRight to squash all numbers to the left
moveLeft :: Board -> Board
moveLeft =  map reverse . moveRight . map reverse

-- Uses left/right to squash all numbers up
moveUp :: Board -> Board
moveUp = transpose . moveLeft . transpose

-- Uses left/right to squash all numbers down
moveDown :: Board -> Board
moveDown = transpose . moveRight . transpose

-- Uses moveX to squash all numbers and subsequently adds a random number to the board using insertRandom
move :: Direction -> Board -> IO Board
move d b =let b' = case d of
			Lef -> moveLeft b
			Rig -> moveRight b
			Up -> moveUp b
			Down -> moveDown b
		in do
			x <- if b==b' then return b else insertRandom b'
			putStrLn (showBoard x)
			return x

