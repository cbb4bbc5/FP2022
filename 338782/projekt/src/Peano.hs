{-# LANGUAGE TemplateHaskell #-}
module Peano(Peano(..),l1,l2,l3,tw1) where
    -- import Formula ( sprintfF, Format((:^:), Lit), Formula, substFormat )
    -- import Axiom ( Axiom(..) )
    import Lib

    import Data.Maybe
    data Peano =
          EqRefl
        | EqElim String Formula
        | PlusZ
        | PlusS
        | Induction String Formula
        deriving (Show,Read)

    instance Axiom Peano where
        axiom EqRefl = read "Ax. x = x"
        axiom (EqElim x f) = makeVar x `seq` sprintfF 
            (Lit ("A"++y++".A"++z++"."++ y ++"="++ z ++ "-> ") :^: substFormat y :^: Lit " -> " :^: substFormat z) f x f x
                where
                    yz = freshVars f
                    y = head yz;z = head $ tail yz
        axiom PlusZ = read "An. (0 + n) = n"
        axiom PlusS = read "An.Am. (S(n) + m) = S(n + m)"
        axiom (Induction x f) = makeVar x `seq` sprintfF 
            (substFormat "0" :^: Lit (" -> (A"++n++".") :^: substFormat n :^: Lit " -> " :^: substFormat ("S("++n++")") :^: Lit (") -> A"++n++".") :^: substFormat n) f x f x f x f x
          where
            n = head $ freshVars f

    $(deriveParse ''Peano)

    l1 :: Theorem
    l1 = proof [] (read "Ax.Ay.x = y -> y = x") & getForalls & intro "ab" & applyThm (fromAxiom (EqElim "b" $ read "b=a") & forallEs ["a","b"]) & trivial & applyAxiom EqRefl & fromJust . qed
    l2 :: Theorem
    l2 = proof [] (read "An.(n+0) = n") & applyAxiom (Induction "n" $ read "(n + 0) = n") & applyAxiom PlusZ & getForall & intro "a0" & applyThm (fromAxiom (EqElim "x" (read "(S(a) + 0) = S(x)")) & forallEs ["a+0","a"])  &  trivial & applyAxiom PlusS & fromJust . qed
    l3 :: Theorem
    l3 = proof [] (read "An.Am. (n + S(m)) = S(n + m)") & getForalls & applyAxiom (Induction "n" $ read "(n + S(b)) = S(n + b)") & applyThm (fromAxiom (EqElim "x" (read "(0 + S(b)) = S(x)")) & forallEs ["b","0 + b"]) & applyThm (l1 & forallEs ["0 + b","b"]) & applyAxiom PlusZ & applyAxiom PlusZ & getForall & intro "a" & applyThm (fromAxiom (EqElim "x" (read "(S(a) + S(b)) = S(x)")) & forallEs ["S(a + b)","S(a) + b"]) & applyThm (l1 & forallEs ["(S(a) + b)","S(a + b)"]) & applyAxiom PlusS & applyThm (fromAxiom (EqElim "x" $ read "x = S(S(a + b))") & forallEs ["S(a + S(b))","S(a) + S(b)"]) & applyThm (l1 & forallEs ["(S(a) + S(b))","S(a + S(b))"]) & applyAxiom PlusS & applyThm (fromAxiom (EqElim "x" $ read "S(a + S(b)) = S(x)") & forallEs ["a + S(b)", "S(a+b)"]) & trivial & applyAxiom EqRefl & fromJust . qed
    tw1 :: Theorem
    tw1 = proof [] (read "An.Am. (n + m) = (m + n)") & getForalls & applyAxiom (Induction "n" $ read "(n + b) = (b + n)") & applyThm (fromAxiom (EqElim "x" $ read "x = (b + 0)") & forallEs ["b", "0 + b"]) & applyThm(l1 & forallEs ["0 + b", "b"]) & applyAxiom PlusZ & applyThm(l1 & forallEs ["b + 0", "b"]) & applyThm l2 & getForall & intro "a" & applyThm (fromAxiom (EqElim "x" $ read "x = b + S(a)") & forallEs ["S(a + b)","S(a) + b"]) & applyThm (l1 & forallEs ["S(a) + b","S(a+b)"]) & applyAxiom PlusS & applyThm (fromAxiom (EqElim "x" $ read "S(x) = b + S(a)") & forallEs ["b + a", "a + b"]) & applyThm (l1 & forallEs ["a + b", "b+a"])& trivial& applyThm (l1 & forallEs ["b+S(a)", "S(b+a)"]) & applyThm l3 & fromJust . qed

        