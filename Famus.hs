--famus
--import Data.List
import System.IO 
import System.IO.Error
import System.Directory



data QA = Q String QA QA
        | A String
         deriving (Show, Read)

testTree = Q "Is she from Europe?" (A "Marie Curie") (A "Marilyn Monroe")
testString = show testTree



errorTest :: IO Bool
errorTest = doesFileExist path


writeQA:: QA -> IO ()
writeQA x = writeFile path (show x)
            

loadQA:: IO QA
loadQA = do s <- readFile path
            return(read s)
 

path :: FilePath
path = "famus.qa"

--question :: String -> IO String

--yesNoQuestion :: String -> IO Bool

main :: IO ()
main = do qa <- if(!errorTest)
		  error "file Does not exist dumbo"
                else
                  loadQA
                  putStrLn ("Think of a famous person! I will ask you questions about her?!?!?!?")
	


--play :: QA -> IO QA
--play qa = 
--          do is <- 





