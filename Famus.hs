import System.Directory

data QA = Q String QA QA
        | A String
         deriving (Show, Read)

startTree = Q "Is she from Europe?" (A "Marie Curie") (A "Marilyn Monroe")

question :: String -> IO String
question q = do putStrLn q
                getLine

yesNoQuestion :: String -> IO Bool
yesNoQuestion q = do auto <- question q
                     if (auto == "yes")
                       then return True
                     else if (auto == "no")
                       then return False
                     else yesNoQuestion q

saveQA :: QA -> IO ()
saveQA x = writeFile path (show x)

loadQA :: IO QA
loadQA = read <$> readFile path

path :: FilePath
path = "famus.qa"

getQA :: IO QA
getQA = do exist <- doesFileExist path
           if exist then loadQA
           else return startTree

main :: IO ()
main = do
  qa <- getQA
  putStrLn ("Think of a famous person! I will ask you questions about her.")
  qa' <- play qa
  saveQA qa'

play :: QA -> IO QA
play (Q s qyes qno) = do
  yes <- yesNoQuestion s
  if yes
    then do t <- play qyes
            return (Q s t qno)
    else do t <- play qno
            return (Q s qyes t)

play (A s) = do
  ans <- yesNoQuestion( "is the person you're thinking about " ++ s )
  if ans
    then do putStrLn "in your face AI is taking over"
            return (A s)
    else do name <- question "I don´t belive you! Who was it!?"
            quest <- question ("Give me a question for which the answer for "
                              ++ name ++ " is yes and the answer for "
                              ++ s ++ " is no")
            return (Q quest (A name) (A s))
