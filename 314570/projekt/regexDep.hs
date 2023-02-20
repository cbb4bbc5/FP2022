{-# LANGUAGE DataKinds, RankNTypes, GADTs, InstanceSigs, TemplateHaskell, UndecidableInstances, TypeFamilies, PolyKinds #-}
module Regex where
import GHC.TypeLits -- provides me implementation for Nat and Symbol kinds
import Data.Singletons
import Data.Singletons.TH

-- $(singletons [d| data ElemType = Integer | String | Mixed deriving (Eq, Ord, Show) |])
-- $(singletons [d| data ElemCount = Singleton | List | Option deriving (Eq, Ord, Show) |])

type ElemList = [(ElemType, ElemCount)]
-- (\w*): (\d).(\d).(\d).(\d)

data RE (s :: ElemList) where
  REmpty :: RE '[]
  RNil :: RE s 
  RLit :: Char -> RE '[]
  RSeq :: RE s1 -> RE s2 -> RE (LinkLists s1 s2)
  RAlt :: RE s1 -> RE s2 -> RE (AltLists s1 s2)
  RStar :: RE s -> RE (StarIter s)


reseq :: RE s1 -> RE s2 -> RE (LinkLists s1 s2)
reseq REmpty re = re
reseq re REmpty = re
reseq RNil _ = RNil
reseq _ RNil = RNil
reseq expr1 expr2 = RSeq expr1 expr2 

realt :: RE s1 -> RE s2 -> RE (AltLists s1 s2)
realt RNil re = re
realt re RNil = re
realt expr1 expr2 = RAlt expr1 expr2

restar :: RE s -> RE (StarIter s)
restar REmpty = REmpty
restar RNil  = RNil
restar expr = ReStar expr 

relit :: Char -> RE '[]
relit = RLit 


-- (\d)|(\w)
-- RE '[ (Integer, Singleton)] = (\d)
-- RE '[ (String, Singleton)] = (\w)
-- RE '[ (Mixed a, Singleton)] = 
type family LinkLists (s1 :: ElemList) (s2 :: ElemList) :: ElemList where
  LinkLists '[] s = s
  LinkLists s '[] = s
  LinkLists (hd : tl) s2 = hd : LinkLists tl s2

-- I don't know how to handle alternation of lists
type family AltLists (s1 :: ElemList) (s2 :: ElemList) :: ElemList where
  AltLists '[] '[] = '[]
  AltLists ( '(t, c) : tl) '[] = '(t, Option) : AltLists tl '[]
  AltLists '[] ( '(t, c) : tl) = '(t, Option) : AltLists '[] tl
  AltLists (hd : tl) s2 = hd : AltLists tl s2


type family StarIter (s :: ElemList) :: ElemList where
  StarIter '[] = '[]
  StarIter ( '(t, c) : _) = '[ '(t, List) ] 


data ListStruct :: ElemList -> * where
  Nil :: ListStruct '[]
  (>:>) :: Elem t c -> ListStruct el -> ListStruct ( '(t, c) : el)


data Elem :: ElemType -> ElemCount -> * where
  E :: forall t c. ElemType_t t -> ElemCount_t c -> Elem t c

type family ElemTypeT (t :: ElemType) = (res :: *) | res -> t where
  ElemTypeT Integer = Int
  ElemTypeT String = String

type family ElemCountT (c :: ElemCount) = (res :: * -> *) | res -> c where
  ElemCountT Option = Maybe 
  ElemCountT Singleton = Id
  ElemCountT Lilst = []
