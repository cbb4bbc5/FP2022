{-# LANGUAGE TemplateHaskell #-}
module Klasyczna(Klass(..)) where
    import Lib
    data Klass = Klass Formula 
    instance Axiom Klass where
        axiom (Klass f) = sprintfF (Lit "~~" :^: FormulaF :^: Lit "->" :^: FormulaF) f f
    $(deriveParse ''Klass)