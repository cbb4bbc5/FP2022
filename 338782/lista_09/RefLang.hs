{-# LANGUAGE TypeFamilies #-}
module RefLang where

import qualified Data.Map as Map
import Control.Monad.IO.Class
import Data.Functor((<&>))

type Var = String

data Expr
  = Var Var           -- zmienne, np. x y z
  | Fun Var Expr      -- lambdy, składnia jak w Haskellu: \ x y z -> 42
  | App Expr Expr     -- aplikacja (jak w Haskellu i OCamlu)
  | Let Var Expr Expr -- let-wyrażenia, np. let x = 42 in f x
  | Ref Expr          -- Tworzenie nowej referencji, np. ref 42
  | Get Expr          -- Dereferencja, np. !x
  | Set Expr Expr     -- Ustawianie wartości komórki pamięci, np. x := 42
  | Seq Expr Expr     -- Sekwencja wyrażeń, np. x := 42; !x
  | Num Integer       -- Stałe liczbowe
  | Inp               -- Wejście (słowo kluczowe input)
  | Out Expr          -- Wyjście: np. output 42
  deriving (Show)

type family Loc (m :: * -> *) :: *

data Value m
  = VNum Integer
  | VFun (Value m -> m (Value m))
  | VLoc (Loc m)

class Monad m => MonadFresh m where
  -- Tworzy świeżą lokację na stercie. Nowej lokacji nie muszą być
  -- przypisane żadne dane.
  freshLoc :: m (Loc m)

class Monad m => MonadHeap m where
  -- Pobiera wartość ze sterty pod podaną lokacją
  heapGet :: Loc m -> m (Value m)
  -- Ustawia wartość na stercie pod podaną lokacją
  heapSet :: Loc m -> Value m -> m ()

type Env m = Map.Map String (Value m)

class MonadFail m => MonadEnv m where
  type EnvValue m :: *
  lookupEnv :: Var -> m (EnvValue m)
  extendEnv :: Var -> EnvValue m -> m a -> m a

eval :: (MonadEnv m, MonadFresh m, MonadHeap m, MonadIO m,EnvValue m ~ Value m) =>
  Expr -> m (Value m)
eval (Var x)   = lookupEnv x
eval (Fun x e) = return $ VFun $ \ v -> extendEnv x v (eval e)
eval (App e1 e2) = do
  VFun f <- eval e1
  v <- eval e2
  f v
eval (Let x e1 e2) = do
  v <- eval e1
  extendEnv x v (eval e2)
eval (Ref e) = do
  v <- eval e
  l <- freshLoc
  heapSet l v
  return (VLoc l)
eval (Get e) = do
  VLoc l <- eval e
  heapGet l
eval (Set e1 e2) = do
  VLoc l <- eval e1
  v <- eval e2
  heapSet l v
  return v
eval (Seq e1 e2) = do
  eval e1
  eval e2
eval (Num n) = return $ VNum n
eval Inp = liftIO getLine <&> VNum . read
eval (Out e) = do
  v <- eval e
  case v of
      VNum x -> liftIO (print x)>>return v
      _ -> error "Not a number"
