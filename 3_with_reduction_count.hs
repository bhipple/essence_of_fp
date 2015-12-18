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
-- 3) State Monad
-- Keep count off the number of reductions that
-- occur while computing the answer
type State = Int

type S a = State -> (a, State)

-- Returns the given state value unchanged
unitS a = \s0 -> (a, s0)

-- Takes a state transformer m :: S a
-- and a function k :: a -> S b
-- Passes the start state to m, which yields
-- a (value,state) pair.  When k is applied to the value,
-- we get a state transformer (k a :: S b), which takes
-- the intermediate state s1 and yields the result
-- and final state pair
m `bindS` k = \s0 -> let (a,s1) = m s0
                         (b,s2) = k a s1
                     in (b,s2)

showS m = let (a,s1) = m 0
          in "Value: " ++ showval a ++ "; " ++
             "Count: " ++ show s1

-- Increment the count. Since this returns the empty
-- tuple, the function is executed for the side-effect only
tickS :: S ()
tickS = \s -> ((), s+1)

type M a = S a
unitM = unitS
bindM = bindS
showM = showS

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
