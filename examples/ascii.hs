-- Print ASCII table (32-126) for TPT R3
infixr 9 .;
infixl 7 * , /;
infixl 6 + , -;
infixr 5 ++;
infix 4 == , <=;
infixl 1 >> , >>=;
infixr 0 $;

data Bool = True | False;

ife a b c = case a of { True -> b ; False -> c };
($) f x = f x;
(.) f g x = f (g x);
flip f x y = f y x;
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where { pure :: a -> f a; (<*>) :: f (a -> b) -> f a -> f b };
class Monad m where { return :: a -> m a; (>>=) :: m a -> (a -> m b) -> m b };
class Eq a where { (==) :: a -> a -> Bool };
class Ord a where { (<=) :: a -> a -> Bool };
instance Eq Int where { (==) = intEq };
instance Ord Int where { (<=) = intLE };

foreign import ccall "putchar" putChar :: Int -> IO Int;

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };

(>>) f g = f >>= \_ -> g;
foldr c n l = flst l n (\h t -> c h (foldr c n t));
(++) = flip (foldr (:));
mapM_ f = foldr ((>>) . f) (pure ());

-- Generate list from a to b
range a b = ife (a <= b) (a : range (a + 1) b) [];

-- Print a character then space, newline every 16 chars
printChar c = putChar c >> putChar 32;

-- Print row of 16 characters starting at n
printRow n = mapM_ printChar (range n (n + 15)) >> putChar 10;

-- Main: print ASCII 32-127 in rows of 16
main = mapM_ printRow [32, 48, 64, 80, 96, 112];
