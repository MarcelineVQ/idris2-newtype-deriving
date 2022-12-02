module Generics.Newtype.Neg

import Generics.Newtype.Num

import Generics.Derive

--------------------------------------------------------------------------------
--          Neg
--------------------------------------------------------------------------------

public export %hint
NegToNumNP : NP (Neg . f) ks -> NP (Num . f) ks
NegToNumNP = mapNP (\_ => materialize Num)

public export %hint
NegToNumPOP : POP (Neg . f) kss -> POP (Num . f) kss
NegToNumPOP = mapPOP (\_ => materialize Num)

-- Derivation of Neg for n-ary products or newtypes
-- e.g. `(a,b,c)` or `data Foo a = MkFoo a`  or  `data Foo = Foo Int`

-- n-ary products, e.g. (a,b,c)   (data Foo x y = MkFoo x y)
public export
NP (Neg . f) ks => Neg (NP f ks) where
  (-) = hcliftA2 (Neg . f) (-)
  negate = hcliftA (Neg . f) negate

-- This type of deriving is for newtypes so we restrict oursellves to single constructor types.
public export
(all : NP (Neg . f) [k']) => Neg (NS f [k']) where
  (-) {all = _::_} (Z x) (Z y) = Z $ x - y
  negate {all = _::_} (Z x) = Z (negate x)

public export
POP (Neg . f) kss => Neg (POP f kss) where
  (-) = hcliftA2 (Neg . f) (-)
  negate = hcliftA (Neg . f) negate

public export
POP (Neg . f) [ks] => Neg (SOP_ k f [ks]) where
  MkSOP x - MkSOP y = MkSOP $ x - y
  negate (MkSOP n) = MkSOP $ negate n

export
genericNegate : Generic t [code] => POP Neg [code] => t -> t
genericNegate = to . negate . from

export
genericSubtract : Generic t [code] => POP Neg [code] => t -> t -> t
genericSubtract x y = to $ (-) (from x) (from y)

export
||| Derives a `Neg` implementation for the given data type
||| and visibility.
NegVis : Visibility -> List Name -> ParamTypeInfo -> List TopLevel
NegVis v _ p =
  let nm := implName p "Neg"
      cl := var nm .= `(MkNeg genericNegate genericSubtract)
   in [TL (interfaceHint v nm (implType "Neg" p)) (def nm [cl])]

export
||| Alias for `NegVis Public`.
Neg : List Name -> ParamTypeInfo -> List TopLevel
Neg = NegVis Public
