import Control.Monad.Random
import Control.Monad.State

main = do
  let blah = do
      x <- readLn
      putStrLn x
    
  blah

increment :: State Int Int
increment = do
  i <- get
  put (i+1)
  return i

sequence' :: [m a] -> m [a]
sequence' [] = return []
sequence' ma:mas = do
  a <- ma
  as <- sequence' mas
  return $ a : as
