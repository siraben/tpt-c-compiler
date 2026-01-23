-- Hello World using string FFI for TPT R3
infixr 9 .;
infixl 1 >>;
infixr 0 $;

($) f x = f x;
(.) f g x = f (g x);

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where { pure :: a -> f a; (<*>) :: f (a -> b) -> f a -> f b };
class Monad m where { return :: a -> m a; (>>=) :: m a -> (a -> m b) -> m b };

foreign import ccall "putchar" putChar :: Int -> IO Int;
foreign import ccall "getchar" getChar :: IO Int;
foreign import ccall "print_string" printString :: Int -> IO Int;

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };

(>>) f g = f >>= \_ -> g;

-- Print string #0 (passed via --string argument to hscompile.py)
main = printString 0;
