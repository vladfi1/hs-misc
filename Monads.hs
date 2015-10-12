class Functor f where
  fmap :: (a -> b) -> f a -> f b

--fmap2 :: (a -> b -> c) -> f a -> f b -> f c

class (Functor f) => Applicative m where
  pure :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b
  
  
  <$> = fmap
  fmap f = (pure f <*>)

f :: a -> b -> c -> d
f <$> a <*> b <*> c :: m d

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
           n a -> (a -> n (m b)) -> n m b
  
  pure = return
  mab <*> ma = mab >>= \ab ->
                              ma >>= \a ->
                                          return (ab a)
  mab <*> ma = do
    ab <- mab
    a <- ma
    return (ab a)
  
type State s a = s -> (s, a)

instance Monad (State s) where
  return a = \s -> (s, a)
--s -> (s, a) >>= (a -> (s -> (s, b))) :: (s -> (s, b))
-- State s a >>= (a -> State s b) = State s b
  f           >>= g = \s -> let (t, a) = f s
                                (u, b) = g a s
                            in (u, b)
  
  get :: State s a
  get = 
  
  put :: s -> State s a
  put =  

join :: m (m a) -> m a
fmap :: m a -> (a -> b) -> m b

>>= = join . fmap

m (n a) <> n (m a)


type Compose f g a = f (g a)

instance (Functor f, Functor g) => Functor (Compose f g) where
--fmap :: (a -> b) -> f (g a) -> f (g b)
--fmap (a -> b) :: g a -> g b
--fmap (g a -> g b) :: f (g a) -> f (g b)
  fmap = fmap . fmap

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = pure . pure
--(<*>) :: f (g (a -> b)) -> f (g a) -> f (g b)
--(<*>) :: g (a -> b) -> g a -> g b
--(<*>) :: f (g a -> g b) -> f (g a) -> f (g b)
--(<*>) fgAB fgA = pure (<*>) <*> fgAB <*> fgA
  
--f (g (a -> b) -> g a -> g b) -> f (g (a -> b)) -> f (g a -> g b) <*> fgA
  
--f (g (a -> b)) -> f (g a -> g b)
  <*> = (pure (<*>) <*>) . <*>
  
instance (Monad m, Monad n) => Monad (Compose m n) where
  (>>=) :: m (n a) -> (a -> m (n b)) -> m (n b)


main = do
  intRef <- newIORef 10
  
  let increment = do
        current <- readIORef intRef
        print current
        writeIORef intRef (current + 1)
  
  increment
  increment
