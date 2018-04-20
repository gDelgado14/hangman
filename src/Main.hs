module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO (hSetBuffering, stdout, BufferMode(..))


type GoalWord = String
type DiscoveredLetters = [Maybe Char]
type UsedLetters = [Char]


data Puzzle =
  Puzzle GoalWord DiscoveredLetters UsedLetters

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ "\n\nGuessed so far: " ++ guessed


type WordList = [String]


minWordLength = 5
maxWordLength = 9


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle


freshPuzzle :: GoalWord -> Puzzle
freshPuzzle word =
  Puzzle word hiddenAnswer []
  where
    hiddenAnswer = map (const Nothing) word


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing  = '_'


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle goal _ _) char = char `elem` goal


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

-- insert guessed char into the
-- user's visible state of the game
-- char can be right or wrong
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle goal visibleState xs) guessChar =
  Puzzle goal newVisibleState (guessChar : xs)

  where
    zipper :: Char -> Maybe Char -> Maybe Char
    zipper wordChar stateChar =
      if wordChar == guessChar then
        Just wordChar
      else
        stateChar

    newVisibleState =
      zipWith zipper goal visibleState


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle char = do
  let guess = toLower char

  putStrLn $ "Your guess was: " ++ [guess]

  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"

      return puzzle

    (True, _) -> do
      putStrLn "Ding Ding Ding!"

      return (fillInCharacter puzzle guess)

    (False, _) -> do
      putStrLn "Errrr! Guess again :)"

      return (fillInCharacter puzzle guess)

-- TODO:
-- throw error if dict.txt not found
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)


gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)

  where
    gameLength w =
      let l = length w
      in
        l >= minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (startBound, endBound)
  return $ wl !! randomIndex

  where
    startBound = 0
    endBound = (length wl) - 1



randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- FIXME:
-- exits even if you guessed right
-- what if goal word is longer than 7 chars?
gameOver :: Puzzle -> IO ()
gameOver (Puzzle goal _ guessed) =
  if (length wrongGuesses) > 4 then
    do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ goal
      exitSuccess
  else
    return ()

  where
    notInGoalWord = not . (flip elem) goal
    wrongGuesses = filter notInGoalWord guessed


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do
      putStrLn "You win!"
      exitSuccess
  else
    return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

