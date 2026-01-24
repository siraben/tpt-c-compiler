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

main =
  -- add/sub/mul/div/mod sanity
  test (60 + 5 == 65) >>
  test (70 - 5 == 65) >>
  test (13 * 5 == 65) >>
  test (130 / 2 == 65) >>
  test (131 `mod` 66 == 65) >>
  test (20 / 3 == 6) >>
  test (20 `mod` 3 == 2) >>
  test (7 * 9 == 63) >>
  -- larger values
  test (1000 + 24 == 1024) >>
  test (1024 - 1000 == 24) >>
  test (32 * 32 == 1024) >>
  test (1024 / 32 == 32) >>
  test (1025 `mod` 32 == 1) >>
  -- width probes
  test (255 + 1 == 256) >>
  test (255 + 1 == 0) >>
  test (65535 + 1 == 0) >>
  putChar 10;
