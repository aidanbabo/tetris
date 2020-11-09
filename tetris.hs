module Main where 

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact

import Data.Maybe
import Control.Monad
import System.Random

-- import System.Random

-- Game Constants
squareSize = 30
gridHeight = 20
gridWidth = 10
windowHeight = squareSize * gridHeight
windowWidth = squareSize * gridWidth

window = InWindow "Tetris" (floor windowWidth, floor windowHeight) (10, 10)
backgroundColor = white
ticksPerSecond = 3

-- Data Types
data Tetris = Tetris StdGen Piece [Brick] deriving Show

data Piece = Piece Point [Brick] deriving Show

data Brick = Brick Float Float Color deriving Show

type PosMap = Float -> Float

-- Main Game Loop
main :: IO ()
main = do
    g <- getStdGen
    play window backgroundColor ticksPerSecond (initialState g) toView eventHandler toNextModel 

-- Starting State of the Game
initialState :: StdGen -> Tetris
initialState g = let (piece, g') = randomFallingPiece g in 
                   Tetris g' piece []

-- Handles all input events
eventHandler :: Event -> Tetris -> Tetris
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) t = move t (subtract 1) id
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) t = move t (+1) id
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) t = move t id (subtract 1)
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) t = fallMax t
eventHandler (EventKey (Char 'a') Down _ _) t = rotCcw t
eventHandler (EventKey (Char 's') Down _ _) t = rotCw t
eventHandler _ t = t

-- Piece Rotations
rotCcw :: Tetris -> Tetris
rotCcw t = rot t rotBrickCcw

rotCw :: Tetris -> Tetris
rotCw t = rot t rotBrickCw

rot :: Tetris -> ((Float,Float) -> [Brick] -> Brick -> Maybe Brick) -> Tetris
rot t@(Tetris g p gbs) f = fromMaybe t $ fmap (\p -> Tetris g p gbs) $ r f p gbs
    where r f (Piece c bricks) gbs = liftM (\bricks -> Piece c bricks) $ sequence $ map (f c gbs) bricks

-- making fun of stupid racket he he
rotBrickCcw :: (Float, Float) -> [Brick] -> Brick -> Maybe Brick
rotBrickCcw (cx,cy) gbs (Brick x y c) = checkBrick ( 
                                          Brick ((+) cx 
                                               ((-) cy 
                                                    y)) 
                                          ((+) cy 
                                               ((-) x 
                                                    cx)) 
                                          c)
                                          gbs

-- Might want to make this real, Also might return None more than once
rotBrickCw :: (Float, Float) -> [Brick] -> Brick -> Maybe Brick
rotBrickCw c gbs b = rotBrickCcw c gbs b >>= (rotBrickCcw c gbs) >>= (rotBrickCcw c gbs)

-- Piece Movement
fallMax :: Tetris -> Tetris
fallMax (Tetris g p gbs) = nextPiece $ Tetris g np gbs
    where np = last $ p : (catMaybes $ takeWhile isJust $ iterate (\p -> p >>= (flip fall) gbs) (Just p))

move :: Tetris -> PosMap -> PosMap -> Tetris
move t@(Tetris gen p gbs) f g = fromMaybe t $ fmap (\p -> Tetris gen p gbs) $ movePiece p gbs f g

movePiece :: Piece -> [Brick] -> PosMap -> PosMap -> Maybe Piece
movePiece (Piece (cx,cy) bricks) gbs fx fy = liftM (\bricks -> Piece (fx cx, fy cy) bricks) $ sequence $ map (moveBrick fx fy gbs) bricks

-- Falling and Clearing and such
toNextModel :: Float -> Tetris -> Tetris
toNextModel _ t@(Tetris g p gbs) = case fall p gbs of
                                 Just p -> Tetris g p gbs
                                 Nothing -> nextPiece t

nextPiece :: Tetris -> Tetris
nextPiece (Tetris g (Piece _ pbs) bs) = let (p, g') = randomFallingPiece g 
                                         in clearRows $ Tetris g' p (bs ++ pbs)

clearRows :: Tetris -> Tetris
clearRows t@(Tetris g p gbs) = t

randomFallingPiece :: StdGen -> (Piece, StdGen)
randomFallingPiece g = let (pieceType, g') = randRangeInt (0, 6) g in
                         piece g' pieceType 
                             where piece g' 0 = let (x, g'') = randRangeInt (0, 8) g' in (o (fromIntegral x) 18, g'')
                                   piece g' 1 = let (x, g'') = randRangeInt (0, 6) g' in (i (fromIntegral x) 19, g'')
                                   piece g' 2 = let (x, g'') = randRangeInt (0, 7) g' in (l (fromIntegral x) 18, g'')
                                   piece g' 3 = let (x, g'') = randRangeInt (0, 7) g' in (j (fromIntegral x) 18, g'')
                                   piece g' 4 = let (x, g'') = randRangeInt (0, 7) g' in (t (fromIntegral x) 18, g'')
                                   piece g' 5 = let (x, g'') = randRangeInt (0, 7) g' in (z (fromIntegral x) 18, g'')
                                   piece g' 6 = let (x, g'') = randRangeInt (0, 7) g' in (s (fromIntegral x) 18, g'')

randRangeInt :: (Int, Int) -> StdGen -> (Int, StdGen)
randRangeInt g r = randomR g r

fall :: Piece -> [Brick] -> Maybe Piece
fall (Piece (cx,cy) bricks) gbs = liftM (\bricks -> Piece (cx,cy-1) bricks) $ sequence $ map (moveBrick id (subtract 1) gbs) bricks

moveBrick :: PosMap -> PosMap -> [Brick] -> Brick -> Maybe Brick
moveBrick fx fy gbs (Brick x y c) = checkBrick (Brick (fx x) (fy y) c) gbs

checkBrick :: Brick -> [Brick] -> Maybe Brick
checkBrick b@(Brick x y c) gbs = if x < 0 || x >= 10 || y < 0 || y >= 19 || any (sameLocation b) gbs then 
                                Nothing
                            else
                                Just $ Brick x y c 

sameLocation :: Brick -> Brick -> Bool
sameLocation (Brick x1 y1 _) (Brick x2 y2 _) = x1 == x2 && y1 == y2

-- Convert Model to View
toView :: Tetris -> Picture
toView (Tetris g fp gbs) = Pictures $ (map brickToView gbs) ++ (pieceToView fp)

pieceToView :: Piece -> [Picture]
pieceToView (Piece _ bricks) = map brickToView bricks

brickToView :: Brick -> Picture
brickToView (Brick x y c) = Translate (-windowWidth / 2) (-windowHeight / 2) $ Color c $ Polygon vertexes
    where vertexes = [ (x * squareSize, y * squareSize) 
                     , ((x+1) * squareSize, y * squareSize) 
                     , ((x+1) * squareSize, (y+1) * squareSize) 
                     , (x * squareSize, (y+1) * squareSize) 
                     ]

-- Functionts to create the different pieces
o :: Float -> Float -> Piece
o x y = Piece (x+0.5, y+0.5) $ shiftsToBricks green x y [(id,id), ((+1),id), ((+1),(+1)), (id,(+1))]

i :: Float -> Float -> Piece
i x y = Piece (x+1.5, y+0.5) $ shiftsToBricks blue x y [(id,id), ((+1),id), ((+2),id), ((+3),id)] 

l :: Float -> Float -> Piece
l x y = Piece (x+1.0, y+0.0) $ shiftsToBricks violet x y [(id,id), ((+1),id), ((+2),id), ((+2),(+1))] 

j :: Float -> Float -> Piece
j x y = Piece (x+1.0, y+0.0) $ shiftsToBricks cyan x y [(id,id), ((+1),id), ((+2),id), (id,(+1))] 

t :: Float -> Float -> Piece
t x y = Piece (x+1.0, y+0.5) $ shiftsToBricks orange x y [(id,id), ((+1),id), ((+2),id), ((+1),(+1))] 

z :: Float -> Float -> Piece
z x y = Piece (x+1.0, y+0.5) $ shiftsToBricks rose x y [(id,(+1)), ((+1),(+1)), ((+1),id), ((+2),id)] 

s :: Float -> Float -> Piece
s x y = Piece (x+1.0, y+0.5) $ shiftsToBricks red x y [(id,id), ((+1),(+1)), ((+1),id), ((+2),(+1))] 

shiftsToBricks :: Color -> Float -> Float -> [(PosMap, PosMap)] -> [Brick]
shiftsToBricks c x y shifts = map (\(fx,fy) -> Brick (fx x) (fy y) c) shifts
