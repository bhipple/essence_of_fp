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
          | At Position Term

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
interp (At p t) e = resetP p (interp t e)

lookup :: Name -> Environment -> M Value
lookup x [] = errorP ("unbound variable: " ++ x)
lookup x ((y,b):e) = if x == y then unitM b else Main.lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add a b = errorP ("should be numbers: " ++ showval a
                  ++ ", " ++ showval b)

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a = errorP ("should be function: " ++ showval f)

test :: Term -> String
test t = showM (interp t [])


-----------------------
-- 0) Standard Interpreter
-- When we provide the identity monad, we get
-- the standard metacircular interpeter for lambda
-- calculus
type I a = a
unitI a = a
a `bindI` k = k a
showI = showval

-- type M a = I a
-- unitM = unitI
-- bindM = bindI
-- showM = showI

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

-- type M a = E a
-- unitM = unitE
-- bindM = bindE
-- showM = showE

-------------------------------------
-- 2) Error messages with positions!
-- We add "At Position Term" to the Term
-- algebraic data type above.

-- Monad P works as the error monad does, but also
-- accepts positions. unitP will discard the position
-- data (since it's valid),, errorP will add the position
-- to the error message, and bindP passes the position along

-- The paper doesn't layout how exactly to define the
-- Position type and how it gets modified as it goes
-- along. Will have to return to this one to think about it.
type Position = String
showpos = show
pos0 = "Start"


type P a = Position -> E a
unitP a = \p -> unitE a
errorP s = \p -> errorE (showpos p ++ ": " ++ s)
m `bindP` k = \p -> m p `bindE` (\x -> k x p)
showP m = showE (m pos0)

-- Function to change position.
-- Discards position p and replaces it with q
resetP :: Position -> P x -> P x
resetP q m = \p -> m q

type M a = P a
unitM = unitP
bindM = bindP
showM = showP

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

terms = [term0, term1, term2, term3]

main = do
        putStrLn "Running tests:"
        mapM_ (putStrLn . test) terms
        putStrLn "Done!"
