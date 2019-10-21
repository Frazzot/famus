--famus
--import Data.List
import System.IO 
import System.IO.Error

data QA = Q String QA QA
        | A String
         deriving (Show, Read)

testTree = Q "Is she from Europe?" (A "Marie Curie") (A "Marilyn Monroe")
testString = show testTree

writeQA:: QA -> IO ()
writeQA x = writeFile path (show x)
            

loadQA:: IO QA
loadQA = do s <- readFile path
            return(read s)
 

path :: FilePath
path = "famus.qa"

--question :: String -> IO String

--yesNoQuestion :: String -> IO Bool

--main :: IO ()


--play :: QA -> IO QA
