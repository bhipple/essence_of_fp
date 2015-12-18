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
          | Count

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
interp Count e = fetchS `bindS` (\i -> unitS (Num i))

lookup :: Name -> Environment -> M Value
lookup x [] = unitM Wrong
lookup x ((y,b):e) = if x == y then unitM b else Main.lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = tickS `bindS` (\() -> unitS (Num (i+j)))
add a b = unitM Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = tickS `bindS` (\() -> k a)
apply f a = unitM Wrong

test :: Term -> String
test t = showM (interp t [])

-----------------
-- 6) Backwards
-- Like the State monad from (3), except now the
-- Count variable will count the number of
-- steps to be perfromed between its evaluation and the
-- end off execution
type State = Int
type S a = State -> (a, State)
unitS a s0 = (a, s0)

-- This is the only change: moving the states
-- It now takes a final state as input and returns an initial
-- state as output!
m `bindS` k = \s2 -> let (a,s0) = m s1
                         (b,s1) = k a s2
                     in (b,s0)

showS m = let (a,s1) = m 0
          in "Value: " ++ showval a ++ "; " ++
             "Count: " ++ show s1

tickS :: S ()
tickS s = ((), s+1)

type M a = S a
unitM = unitS
bindM = bindS
showM = showS

fetchS :: S State
fetchS s = (s,s)

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

-- Adds the number of reductions to the result
-- of adding one a few times.  Neat!
term4 = Add (Add (Add (Con 1) (Con 1)) (Con 1)) Count

terms = [term0, term1, term2, term3, term4]

main = do
        putStrLn "Running tests:"
        mapM_ (putStrLn . test) terms
        putStrLn "Done!"
