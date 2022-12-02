module Generics.Newtype.Fractional

import Generics.Newtype.Num

import Generics.Derive

--------------------------------------------------------------------------------
--          Fractional
--------------------------------------------------------------------------------

public export %hint
FractionalToNumNP : NP (Fractional . f) ks -> NP (Num . f) ks
FractionalToNumNP = mapNP (\_ => materialize Num)

public export %hint
FractionalToNumPOP : POP (Fractional . f) kss -> POP (Num . f) kss
FractionalToNumPOP = mapPOP (\_ => materialize Num)

-- Derivation of Fractional for n-ary products or newtypes
-- e.g. `(a,b,c)` or `data Foo a = MkFoo a`  or  `data Foo = Foo Int`

-- n-ary products, e.g. (a,b,c)   (data Foo x y = MkFoo x y)
public export
NP (Fractional . f) ks => Fractional (NP f ks) where
  (/) = hcliftA2 (Fractional . f) (/)
  recip = hcliftA (Fractional . f) recip

-- This type of deriving is for newtypes so we restrict oursellves to single constructor types.
public export
(all : NP (Fractional . f) [k']) => Fractional (NS f [k']) where
  (/) {all = _::_} (Z x) (Z y) = Z $ x / y
  recip {all = _::_} (Z x) = Z (recip x)

public export
POP (Fractional . f) kss => Fractional (POP f kss) where
  (/) = hcliftA2 (Fractional . f) (/)
  recip = hcliftA (Fractional . f) recip

public export
POP (Fractional . f) [ks] => Fractional (SOP_ k f [ks]) where
  MkSOP x / MkSOP y = MkSOP $ x / y
  recip (MkSOP n) = MkSOP $ recip n

export
genericDivFractional : Generic t [code] => POP Fractional [code] => t -> t -> t
genericDivFractional x y = to $ (/) (from x) (from y)

export
genericRecip : Generic t [code] => POP Fractional [code] => t -> t
genericRecip = to . recip . from

export
||| Derives a `Fractional` implementation for the given data type
||| and visibility.
FractionalVis : Visibility -> List Name -> ParamTypeInfo -> List TopLevel
FractionalVis v _ p =
  let nm := implName p "Fractional"
      cl := var nm .= `(MkFractional genericDivFractional genericRecip)
   in [TL (interfaceHint v nm (implType "Fractional" p)) (def nm [cl])]

export
||| Alias for `FractionalVis Public`.
Fractional : List Name -> ParamTypeInfo -> List TopLevel
Fractional = FractionalVis Public
