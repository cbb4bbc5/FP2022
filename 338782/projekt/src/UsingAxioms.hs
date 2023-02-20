{-# LANGUAGE DataKinds, TypeFamilies  #-}
module UsingAxioms(AllAxioms) where
    import Axiom ( Axioms )
    import Peano ( Peano )
    import Klasyczna ( Klass )
    import ZFC
    type AllAxioms = Axioms '[Peano,Klass,ZFC]    