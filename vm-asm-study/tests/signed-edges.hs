-- Arithmetic tests: prints a sequence of T/F flags, then newline.
-- Each test outputs 'T' if the comparison holds, else 'F'.
infixr 9 .;
infixl 7 * , /;
infixl 6 + , -;
infixr 5 ++;
infix 4 ==;
infixl 1 >>;
infixr 0 $;
(+) = intAdd;
(-) = intSub;
(*) = intMul;
(/) = intDiv;
mod = intMod


($) f x = f x;
(.) f g x = f (g x);
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where { pure :: a -> f a; (<*>) :: f (a -> b) -> f a -> f b };
class Monad m where { return :: a -> m a; (>>=) :: m a -> (a -> m b) -> m b };
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Int where { (==) = intEq };

foreign import ccall "putchar" putChar :: Int -> IO Int;

instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };

(>>) f g = f >>= \_ -> g;

test b = case b of { True -> putChar 84; False -> putChar 70 };

main = test (intLE (0-1) 0) >>
  test (intLE (0-32768) (0-1)) >>
  test (intEq (32767+1) (0-32768)) >>
  test (intEq (65535+1) 0) >>
  test (intEq ((0-7)/3) (0-2)) >>
  test (intEq (7/(0-3)) (0-2)) >>
  test (intEq ((0-7)/(0-3)) 2) >>
  test (intEq ((0-7) `mod` 3) (0-1)) >>
  test (intEq (7 `mod` (0-3)) 1) >>
  test (intEq ((0-32768)/1) (0-32768)) >>
  test (intEq ((0-32768)/(0-1)) (0-32768)) >>
  test (intEq (32767/256) 127) >>
  test (intEq (32767 `mod` 256) 255) >> putChar 10;
