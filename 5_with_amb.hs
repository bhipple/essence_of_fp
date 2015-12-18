module Main where
import Data.List
-- Interpreter for lambda calculus

-- Three functions for working with generic monads.
-- unitM coerces a value into the computation monad,
-- bindM lifts a value from one monad into another,
-- and showM gets us out of the monad to see just the value
unitM :: a -> M a
bindM :: M a -> (a -> M b) -> M b
showM :: M Value -> String

-- The code for the interpreter. Plugging different monads
-- into the same generic base gives greatly varying behavior
type Name = String
data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Fail
          | Amb Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> M Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e = Main.lookup x e
interp (Con i) e = unitM (Num i)
interp (Add u v) e = interp u e `bindM` (\a ->
                     interp v e `bindM` (\b ->
                     add a b))
interp (Lam x v) e = unitM (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindM` (\f ->
                     interp u e `bindM` (\a ->
                     apply f a))
interp Fail e = zeroL
interp (Amb u v) e = interp u e `plusL` interp v e

lookup :: Name -> Environment -> M Value
lookup x [] = unitM Wrong
lookup x ((y,b):e) = if x == y then unitM b else Main.lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add a b = unitM Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a = unitM Wrong

test :: Term -> String
test t = showM (interp t [])


--------------------------------------------------
-- 1) Non-deterministic Choice with the List Monad

type L a = [a]

unitL a = [a]
m `bindL` k = [b | a <- m, b <- k a]

zeroL = []
l `plusL` m = l ++ m

showL m = "[" ++ intercalate "," [showval a | a <- m] ++ "]"

type M a = L a
unitM = unitL
bindM = bindL
showM = showL

----------------------
-- To be used as a test:
-- ((\x.x + x) (10 + 11)) => 42
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Con 10) (Con 11))

-- Invalid syntax: Trying to apply integers
term1 = App (Con 1) (Con 2)

-- ((\x.x + x) 3)
term2 = App
            (Lam "x" (Add (Var "x") (Var "x")))
            (Con 3)

term3 = Con 5

term4 = App (Lam "x" (Add (Var "x") (Var "x"))) (Amb (Con 1) (Con 2))

terms = [term0, term1, term2, term3, term4]

main = do
        putStrLn "Running tests:"
        mapM_ (putStrLn . test) terms
        putStrLn "Done!"
