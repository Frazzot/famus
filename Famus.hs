import System.Directory
import System.IO
import Control.Monad

data QA = Q String QA QA
        | A String
         deriving (Show, Read)

defaultQA = Q "Is she from Europe?" (A "Marie Curie") (A "Marilyn Monroe")

question :: String -> IO String
question q = do putStr (q ++ " ")
                hFlush stdout
                getLine

-- Takes a string and returns an IO Bool to represent the answer of the question
yesNoQuestion :: String -> IO Bool
yesNoQuestion q = do
  answer <- question q
  case answer of "yes" -> return True
                 "no"  -> return False
                 _     -> yesNoQuestion "Answer me yes or no!"

saveQA :: QA -> IO ()
saveQA x = writeFile path $ show x

-- Basically maps read infix on readFile path
loadQA :: IO QA
loadQA = read <$> readFile path

-- Returns the path to the file
path :: FilePath
path = "famus.qa"

-- Calls loadQA if file path exists, otherwise it returns the defaultQA
getQA :: IO QA
getQA = do exist <- doesFileExist path
           if exist then loadQA
             else return defaultQA

-- The main function of the game, handles when other functions should be called and the progression of the game
main :: IO ()
main = do
  qa <- getQA
  putStrLn "Think of a famous person! I will ask you questions about her."
  qa' <- play qa
  saveQA qa'
  again <- yesNoQuestion "Play again?"
  if again then main
    else putStrLn "OK! Bye pesky human!"

play :: QA -> IO QA
play (Q s qyes qno) = do
  yes <- yesNoQuestion s
  if yes
    then do t <- play qyes
            return (Q s t qno)
    else do t <- play qno
            return (Q s qyes t)

play (A s) = do
  yes <- yesNoQuestion ("Is the person you're thinking about " ++ s ++ "?")
  if yes
    then do putStrLn "In your face! AI is taking over!"
            return (A s)
    else do name <- question "I don´t belive you! Who was it!?"
            putStrLn ("I`ve never heard of " ++ name ++ "... " ++
                     "I told you to think of someone famous right? But OK...")
            quest <- question ("Give me a question for which the answer for "
                              ++ name ++ " is yes and the answer for "
                              ++ s ++ " is no.\n>")
            return (Q quest (A name) (A s))
