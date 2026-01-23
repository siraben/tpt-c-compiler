-- Factorial for TPT R3
infixr 9 .;
infixl 7 * , /;
infixl 6 + , -;
infixr 5 ++;
infix 4 ==;
infixl 1 >>;
infixr 0 $;

data Bool = True | False;

($) f x = f x;
(.) f g x = f (g x);
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where { pure :: a -> f a; (<*>) :: f (a -> b) -> f a -> f b };
class Monad m where { return :: a -> m a; (>>=) :: m a -> (a -> m b) -> m b };
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Int where { (==) = intEq };
(+) = intAdd;
(-) = intSub;
(*) = intMul;
(/) = intDiv;

foreign import ccall "putchar" putChar :: Int -> IO Int;

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };

(>>) f g = f >>= \_ -> g;
foldr c n l = flst l n (\h t -> c h (foldr c n t));
mapM_ f = foldr ((>>) . f) (pure ());

-- Factorial function
fact n = case n of { 0 -> 1; _ -> n * fact (n - 1) };

-- Convert number to list of digit characters (ASCII)
-- 6! = 720, so we need to print "720\n"
-- We'll compute the digits by repeated division
divMod n d = (n / d, n - (n / d) * d);

toDigits n = case n of {
  0 -> [];
  _ -> case divMod n 10 of { (q, r) -> toDigits q ++ [r + 48] }
};

toDigits' n = case n of { 0 -> [48]; _ -> toDigits n };

(++) xs ys = foldr (:) ys xs;

-- Main: print 6! (720) followed by newline
main = mapM_ putChar (toDigits' (fact 6) ++ [10]);
