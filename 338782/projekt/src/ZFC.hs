{-# LANGUAGE TemplateHaskell #-}
module ZFC(ZFC(..)) where
    import Lib
    data ZFC =
          Ext -- Aa.Ab.(Ax. x@a <=> y@a) -> a = b
        | Reg -- Ax. (Ey. y@x) -> Ey. y@x & ~Ez. z@x & z@y
        -- | Empty -- Ex.Ay. ~y@x
        | Pair -- Aa.Ab.Ec.Ax. x@c <=> x=a | x=b
        | Union -- Ar.Eu.Ax. x@u <=> Ea. x@a & a@r
        | Replace String String String Formula -- AA. (Ax. x@A -> Ey! Formula{->x,->y}) -> E(B).Ay. y@B <=> Ex. x@A & Formula{->x,->y}
        | Inf -- Ex. (Ea. a @ x & (Ab.~b@a)) & Ac. c@x -> Ed. d@x & Ae. e@d <=> e@c & e = c
        | Power -- AX. EPX. Apx. px @ PX <=> Ax. x @ px -> x @ X
        | Choise -- AX.(AA.A @ X -> Ea.a @ A) -> EY. AA. A @ X -> Ey! y @ Y & y @ A


    instance Axiom ZFC where
      axiom Ext = read "Aa.Ab.(Ax. x@a <=> y@a) -> a = b"
      axiom Reg = read "Ax. (Ey. y@x) -> Ey. y@x & ~Ez. z@x & z@y"
      axiom Pair = read "Aa.Ab.Ec.Ax. x@c <=> x=a | x=b"
      axiom Union = read " Ar.Eu.Ax. x@u <=> Ea. x@a & a@r"
      axiom (Replace a x y f) = makeVar x `seq` makeVar y `seq` makeVar a `seq`
          sprintfF (Lit ("A"++a++". (A"++x++". "++x++"@"++a++" -> E"++y++"! ") :^: FormulaF :^: Lit ("-> E("++b++").A"++y++". "++y++"@"++b++" <=> E"++x++". "++x++"@"++a++" & "):^:FormulaF) f f
        where
          b = head $ freshVars f
      axiom Inf = read "Ex. (Ea. a @ x & (Ab.~b@a)) & Ac. c@x -> Ed. d@x & Ae. e@d <=> e@c & e = c"
      axiom Power = read "AX. EPX. Apx. px @ PX <=> Ax. x @ px -> x @ X"
      axiom Choise = read "AX.(AA.A @ X -> Ea.a @ A) -> EY. AA. A @ X -> Ey! y @ Y & y @ A"

    $(deriveParse ''ZFC)