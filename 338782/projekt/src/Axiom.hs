{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Axiom (Axiom(axiom),TwoAxioms(..),Axioms) where
    import Formula
    import Text.Read(readPrec,(+++))
    import Data.Kind
    class Axiom a where
        axiom :: a -> Formula

    data TwoAxioms a b = FirstAxiom a | SecondAxiom b
 
    type family Axioms (a :: [Type]) :: Type
    type instance Axioms (x ': '[]) = x
    type instance Axioms (x ': y ': xs) = TwoAxioms x (Axioms (y ': xs))
    
    instance (Read a,Read b) => Read (TwoAxioms a b) where
        readPrec = fmap FirstAxiom readPrec +++ fmap SecondAxiom readPrec

    instance (Axiom a,Axiom b) => Axiom (TwoAxioms a b) where
        axiom (FirstAxiom a) = axiom a
        axiom (SecondAxiom a) = axiom a
