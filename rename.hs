import System.IO  
import System.Directory  
import Data.List  
  
main = do        
    handle <- openFile "test1.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    removeFile "test1.txt"  
    renameFile tempName "todo.txt" 
