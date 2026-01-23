-- String reversal program for TPT R3 computer
-- Reads input until newline, then outputs the reversed string

infixr 9 .;
infixl 7 *;
infixl 6 + , -;
infixr 5 ++;
infix 4 == , <=;
infixl 3 &&;
infixl 2 ||;
infixl 1 >> , >>=;
infixr 0 $;

data Bool = True | False;

ife a b c = case a of { True -> b ; False -> c };
not a = case a of { True -> False; False -> True };
($) f x = f x;
(.) f g x = f (g x);
flip f x y = f y x;
(&&) f g = ife f g False;
(||) f g = ife f True g;

fpair p = \f -> case p of { (,) x y -> f x y };
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where
{ pure :: a -> f a
; (<*>) :: f (a -> b) -> f a -> f b
};
class Monad m where
{ return :: a -> m a
; (>>=) :: m a -> (a -> m b) -> m b
};
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Int where { (==) = intEq };

foreign import ccall "putchar" putChar :: Int -> IO Int;
foreign import ccall "getchar" getChar :: IO Int;

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };

(>>) f g = f >>= \_ -> g;

foldr c n l = flst l n (\h t -> c h (foldr c n t));
(++) = flip (foldr (:));
map f = foldr ((:) . f) [];
foldl f a bs = foldr (\b g x -> g (f x b)) (\x -> x) bs a;
reverse = foldl (flip (:)) [];

mapM_ f = foldr ((>>) . f) (pure ());

-- Read characters until newline (code 10)
getLine = getChar >>= \c -> ife (c == 10) (pure []) (getLine >>= \cs -> pure (c : cs));

-- Print a list of characters
putStr = mapM_ putChar;

-- Main: read a line, reverse it, print it with newline
main = getLine >>= \s -> putStr (reverse s) >> putChar 10;
