module Main where
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

lookup :: Name -> Environment -> M Value
lookup x [] = errorE ("unbound variable: " ++ x)
lookup x ((y,b):e) = if x == y then unitM b else Main.lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add a b = errorE ("should be numbers: " ++ showval a
                  ++ ", " ++ showval b)

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a = errorE ("should be function: " ++ showval f)

test :: Term -> String
test t = showM (interp t [])

-------------------------
-- 1) With Error Messages
-- This only requires the invalid cases of
-- lookup, add, and apply substituting
-- errorE instead of Wrong, and adding some new
-- data types. This simulates error handling that
-- we could get with exceptions or continuations.
data E a = Success a | Error String

unitE = Success
errorE = Error

(Success a) `bindE` k = k a
(Error s) `bindE` k = Error s

showE (Success a) = "Success: " ++ showval a
showE (Error s) = "Error: " ++ s

type M a = E a
unitM = unitE
bindM = bindE
showM = showE

----------------------
-- To be used as a test:
-- ((\x.x + x) (10 + 11)) => 42
term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Con 10) (Con 11))

-- Invalid syntax: Trying to apply integers
term1 = App (Con 1) (Con 2)

main = do
        putStrLn "Running tests:"
        mapM_ (putStrLn . test) [term0, term1]
        putStrLn "Done!"
