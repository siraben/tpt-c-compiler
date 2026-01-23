-- Simple hello world for TPT R3
infixr 9 .;
infixl 1 >>;
infixr 0 $;

($) f x = f x;
(.) f g x = f (g x);
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where { pure :: a -> f a; (<*>) :: f (a -> b) -> f a -> f b };
class Monad m where { return :: a -> m a; (>>=) :: m a -> (a -> m b) -> m b };

foreign import ccall "putchar" putChar :: Int -> IO Int;

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };

(>>) f g = f >>= \_ -> g;
foldr c n l = flst l n (\h t -> c h (foldr c n t));
mapM_ f = foldr ((>>) . f) (pure ());

-- Print "Hi" (72 = 'H', 105 = 'i')
main = putChar 72 >> putChar 105 >> putChar 10;
