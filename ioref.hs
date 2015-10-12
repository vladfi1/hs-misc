import Data.IORef

main = do
  intRef <- newIORef 10
  
  let increment = do
        current <- readIORef intRef
        print current
        writeIORef intRef (current + 1)
  
  increment
  increment

