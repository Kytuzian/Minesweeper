import Library.Math
import Library.List
import Library.Random

import Click (click, longClick)

import Control.Monad
import Control.Concurrent
import Codec.Picture
import System.Process

import Data.List
import Data.Maybe
import Data.Ratio
import Data.Word

x `isInRange` (l, h) = x >= l && x < h

data Square = Open | Bomb | Unclicked | Surrounded Int
    deriving (Eq)

instance Show Square where
    show Bomb = "*"
    show Open = "_"
    show (Surrounded n) = show n
    show Unclicked = "#"

data Board = Board Int Int [[Square]]
    deriving (Eq)

instance Show Board where
    show (Board _ _ rows) = intercalate "\n" $ map (intercalate " " . map show) rows

takeMacScreenshot :: String -> IO ()
takeMacScreenshot fname = do
    r <- readProcess "screencapture" [fname] ""
    return ()

colorNums = [(PixelRGBA8 0 0 255 255, Surrounded 1), (PixelRGBA8 0 153 0 255, Surrounded 2),
             (PixelRGBA8 255 0 0 255, Surrounded 3), (PixelRGBA8 102 102 178 255, Surrounded 4),
             (PixelRGBA8 153 0 0 255, Surrounded 5), (PixelRGBA8 0 153 153 255, Surrounded 6),
             (PixelRGBA8 255 255 255 255, Unclicked), (PixelRGBA8 0 0 0 255, Bomb)]
defaultReadBoard = do
    v <- findImg "checkim.png" "board.png"

    case v of
        Nothing -> do
            putStrLn "Couldn't find the board!"
            return $ makeBlankBoard 10 10
        Just (x, y) -> do
            -- print (x + 14, y + 15)
            readBoard (PixelRGBA8 204 204 204 255) colorNums (x + 14, 30) (y + 15, 16) (23.0 + 1.0 / 3.0, 23.0 + 3 / 16)

getR :: PixelRGBA8 -> Word8
getR (PixelRGBA8 r _ _ _) = r

getG :: PixelRGBA8 -> Word8
getG (PixelRGBA8 _ g _ _) = g

getB :: PixelRGBA8 -> Word8
getB (PixelRGBA8 _ _ b _) = b

getMatchPos :: Eq a => [a] -> [a] -> Maybe Int
getMatchPos match check = getMatchPos' check 0
    where getMatchPos' [] _ = Nothing
          getMatchPos' c@(_:cs) i
            | match == take ml c = Just i
            | otherwise = getMatchPos' cs (i + 1)
          ml = length match

getMatchPos2 :: (Show a, Eq a) => [[a]] -> [[a]] -> Maybe (Int, Int)
getMatchPos2 match@(m1:ms) check
    | cw < mw || ch < mh = Nothing
    | otherwise = getMatchPos' check 0
    where getMatchPos' [] _ = Nothing
          getMatchPos' c@(c1:cs) y = case getMatchPos m1 c1 of
                                        Just x -> case (map (take mw . drop x) $ take mh c) == match of
                                                    True -> Just (x, y)
                                                    False -> getMatchPos' cs (y + 1)
                                        Nothing -> getMatchPos' cs (y + 1)
          (mw, mh) = dims match
          (cw, ch) = dims check

imgArray :: Pixel a => Image a -> [[a]]
imgArray img = [[pixelAt img x y | x <- [0..imageWidth img - 1]] | y <- [0..imageHeight img - 1]]

findImg :: FilePath -> FilePath -> IO (Maybe (Int, Int))
findImg p1 p2 = do
    timg1 <- readPng p1
    case timg1 of
        Left err -> return Nothing
        Right (ImageRGBA8 img1) -> do
            timg2 <- readPng p2
            case timg2 of
                Left err -> return Nothing
                Right (ImageRGBA8 img2) -> do
                    let img1Arr = imgArray img1
                    let img2Arr = imgArray img2

                    return $ getMatchPos2 img1Arr img2Arr

readBoard :: PixelRGBA8 -> [(PixelRGBA8, Square)] -> (Int, Int) -> (Int, Int) -> (Float, Float) -> IO Board
readBoard ignoreColor colors xbound@(xmin, squaresInRow) ybound@(ymin, squaresInColumn) (sw, sh) = do
    takeMacScreenshot "board.png"
    res <- readPng "board.png"
    case res of
        Left err -> do
            putStrLn $ "Couldn't read the image" ++ err
            return $ makeBlankBoard 10 10
        Right (ImageRGBA8 img) -> do
            let test = replicate squaresInColumn (replicate squaresInRow Open)

            let allPixels = [(pixelAt img x y, x, y) | y <- [ymin..ymax - 1], x <- [xmin..xmax - 1],
                                                       let (tx, ty) = convert x y, tx `isInRange` (0, squaresInRow),
                                                       ty `isInRange` (0, squaresInColumn)]
            let foundColors = filter (\(color, _, _) -> color `elem` (map fst colors)) allPixels
            let finalSquares = foldl (\squares (color, x, y) -> setAt2 squares (convert x y) (getSquare color)) test foundColors

            -- print $ sort $ countDuplicates [pixelAt img x y | y <- [ymin..ymax - 1], x <- [xmin..xmax - 1],
            --                                            x `isInRange` (756, 756+23),
            --                                            y `isInRange` (293, 293+23),
            --                                            let pixel = pixelAt img x y,
            --                                            getR pixel /= getB pixel || getR pixel /= getG pixel]

            return $ Board squaresInRow squaresInColumn finalSquares
        Right _ -> do
            putStrLn "Unknown image format."
            return $ makeBlankBoard 10 10
    where iw = xmax - xmin
          ih = ymax - ymin
          xmax = round (fromIntegral xmin + sw * fromIntegral squaresInRow - 1)
          ymax = round (fromIntegral ymin + sh * fromIntegral squaresInColumn - 1)
          xs = map round [0, sw..fromIntegral iw]
          ys = map round [0, sh..fromIntegral ih]
          -- Get the most common color in the box (as long as the color is in the list of colors above)
          -- Returns the number associated with that color
          convert x y = ((x - xmin) `div` (round sw), (y - ymin) `div` (round sh))
          getSquare color = case find ((== color). fst) colors of
                                  Just (_, v) -> v
                                  Nothing -> Open

-- Plays a game on the screen
playGame :: Int -> Int -> IO ()
playGame xmin ymin = do
    board@(Board w h rows) <- readBoard squareBackground colorNums (xmin, squaresInRow) (ymin, squaresInColumn) squareSize
    case count (== Bomb) $ concat rows of -- Show be none, unless we lost
        0 -> case find (== Unclicked) $ concat rows of
                Just x -> do
                    putStrLn "--------------------------------"
                    let markedBoard@(Board _ _ markedRows) = markBombs board
                    print markedBoard
                    case count (== Unclicked) $ concat markedRows of
                        0 -> putStrLn "Won the game!"
                        _ -> do
                            allClicks <- case findAllM (isSafe markedBoard) squares of
                                                [] -> do
                                                    let unclickedSquares = filter ((== Unclicked) . (markedRows !!!)) squares
                                                    i <- randIndex unclickedSquares
                                                    return [Just $ unclickedSquares !! i]
                                                xs -> return xs
                            clickPositions <- mapM getClickPosition allClicks

                            -- Do the square clicking here
                            mapM (\(screenX, screenY) -> click screenX screenY) clickPositions

                            threadDelay 3000000
                            playGame xmin ymin
                Nothing -> do
                    putStrLn "--------------------------------"
                    print board
                    putStrLn "Won the game!"
        _ -> do
            putStrLn "Lost the game!"
            click (xmin + 245) (ymin + 405) -- Restart button
            playGame xmin ymin
    where getClickPosition v = do
                case v of
                    Just (x, y) -> do
                        let screenX = round (fromIntegral x * sw) + xmin + (round $ sw / 2)
                        let screenY = round (fromIntegral y * sh) + ymin + (round $ sh / 2)

                        return (screenX, screenY)
                    Nothing -> return (xmin, ymin)
          squares = [(x, y) | x <- [0..squaresInRow - 1], y <- [0..squaresInColumn - 1]]
          squareBackground = (PixelRGBA8 204 204 204 255)
          squaresInRow = 30
          squaresInColumn = 16
          squareSize@(sw, sh) = (23.0 + 1.0 / 3.0, 23.0 + 3.0 / 16.0)
          xmax = round (fromIntegral xmin + sw * fromIntegral squaresInRow - 1)
          ymax = round (fromIntegral ymin + sh * fromIntegral squaresInColumn - 1)

makeBlankBoard :: Int -> Int -> Board
makeBlankBoard w h = Board w h (replicate h (replicate w Unclicked))

findAllM :: (a -> Maybe Bool) -> [a] -> [Maybe a]
findAllM _ [] = []
findAllM f (x:xs) = case f x of
                        Just True -> Just x : findAllM f xs
                        Just False -> findAllM f xs
                        Nothing -> findAllM f xs

findM :: (a -> Maybe Bool) -> [a] -> Maybe a
findM _ [] = Nothing
findM f (x:xs) = case f x of
                    Just True -> Just x
                    Just False -> findM f xs
                    Nothing -> findM f xs

generateBoard :: Int -> Int -> Int -> IO Board
generateBoard w h bombs = do
    bombPositions <- replicateM (bombs^2) bombPosition
    let realBombPos = take bombs $ nub bombPositions -- Cause I'm lazy.
    let boardBombs = foldl (\board pos -> setAt2 board pos Bomb) (replicate h (replicate w Unclicked)) realBombPos
    let finalBoard = foldl (\board pos -> setAt2 board pos $ getSquareType board pos) boardBombs [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]

    return $ Board w h finalBoard
    where bombPosition = do
                    x <- randInt 0 (w - 1)
                    y <- randInt 0 (h - 1)
                    return (x, y)
          getSquareType board (x, y)
            | board !!! (x, y) == Bomb = Bomb
            | bombsAround > 0 = Surrounded bombsAround
            | otherwise = Open
            where bombsAround = neighborsOf (Board w h board) Bomb (x, y)

test :: Int -> Int -> Int -> IO ()
test w h bombs = do
    startingBoard <- generateBoard w h bombs
    testPlayGame (makeBlankBoard w h) startingBoard

testPlayGame :: Board -> Board -> IO ()
testPlayGame board@(Board _ _ curRows) actualBoard@(Board w h actualRows) = do
    case find (== Unclicked) $ concat curRows of
        Just x -> do
                    -- putStrLn "--------------------------------"
                    putStrLn "--------------------------------"
                    let markedBoard = markBombs board
                    print markedBoard
                    (x, y) <- case findM (isSafe markedBoard) squares of
                                Just (x, y) -> return (x, y)
                                Nothing -> do
                                    i <- randIndex squares
                                    return $ squares !! i

                    case actualRows !!! (x, y) of
                        Bomb -> putStrLn "Lost the game!"
                        _ -> testPlayGame (clickSquare actualBoard markedBoard (x, y)) actualBoard
        Nothing -> do
                    putStrLn "--------------------------------"
                    print board
                    putStrLn "Won the game!"
    where squares = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]

clickSquare :: Board -> Board -> (Int, Int) -> Board
clickSquare realBoard@(Board _ _ realRows) currentBoard@(Board w h currentRows) (x, y) = case realRows !!! (x, y) of
        Surrounded n -> nextBoard
        _ -> foldl (clickSquare realBoard) nextBoard clickedNeighbors
    where nextBoard = Board w h $ setAt2 currentRows (x, y) $ realRows !!! (x, y)
          clickedNeighbors = filter (\(x, y) -> (realRows !!! (x, y) == Open || isSurrounded (realRows !!! (x, y))) && currentRows !!! (x, y) == Unclicked) $ neighbors realBoard (x, y)

boardRows :: Board -> [[Square]]
boardRows (Board _ _ rows) = rows

isSurrounded :: Square -> Bool
isSurrounded (Surrounded _) = True
isSurrounded _ = False

neighbors :: Board -> (Int, Int) -> [(Int, Int)]
neighbors (Board w h _) (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
                                             (x + dx) `isInRange` (0, w),
                                             (y + dy) `isInRange` (0, h),
                                             dx /= 0 || dy /= 0]

neighborsOf :: Board -> Square -> (Int, Int) -> Int
neighborsOf board@(Board _ _ rows) square (x, y) = count ((== square) . (rows !!!)) $ neighbors board (x, y)

-- Returns the number of bombs left if the square is a surrounded square.
-- If it is not, returns Nothing
bombsLeft :: Board -> (Int, Int) -> Maybe Int
bombsLeft board@(Board _ _ rows) (x, y) = case rows !!! (x, y) of
                            (Surrounded n) -> Just (n - bombNeighbors)
                            _ -> Nothing
    where bombNeighbors = neighborsOf board Bomb (x, y)

-- If it is a surrounded square, returns whether all bombs have been found or not
-- If it is not a surrounded square, returns False
isDone :: Board -> (Int, Int) -> Bool
isDone board (x, y) = case bombsLeft board (x, y) of
                        Just n -> n == 0
                        Nothing -> False

-- Returns true if all unclicked squares surrounding a square must be bombs
unclickedAreBombs :: Board -> (Int, Int) -> Bool
unclickedAreBombs board (x, y) = case (boardRows board) !!! (x, y) of
                                    Surrounded n -> case bombsLeft board (x, y) of
                                                        Just nLeft -> neighborsOf board Unclicked (x, y) == nLeft
                                                        Nothing -> undefined -- Shouldn't happen
                                    _ -> False

markBombs :: Board -> Board
markBombs board@(Board w h rows) = Board w h $ foldl (\curRows (x, y) -> setAt2 curRows (x, y) Bomb) rows bombs
    where bombs = filter (not . (fromMaybe True) . (isSafe board)) possibleSquares
          possibleSquares = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]

-- Determines whether an unclicked square is safe to click
-- If the square is safe, will return Just True, if not, Just False.
-- If it cannot be determined, or the square has already been determined, it will return Nothing.
isSafe :: Board -> (Int, Int) -> Maybe Bool
isSafe board@(Board _ _ rows) (x, y)
    | square == Unclicked = squareIsSafe
    | square `elem` [Bomb, Open] = Nothing
    | isSurrounded square = Nothing
    where square = rows !!! (x, y)
          squareNeighbors = neighbors board (x, y)
          squareIsSafe
            | any (isDone board) squareNeighbors = Just True
            | squareIsBomb = Just False
            | otherwise = Nothing
            where squareIsBomb = any (unclickedAreBombs board) squareNeighbors

main = do
    takeMacScreenshot "board.png"
    v <- findImg "checkim.png" "board.png"

    case v of
        Nothing -> putStrLn "Couldn't find the board!"
        Just (x, y) -> do
            playGame (x + 14) (y + 15)
