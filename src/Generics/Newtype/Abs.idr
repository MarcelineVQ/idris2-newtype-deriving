module Generics.Newtype.Abs

import Generics.Newtype.Num

import Generics.Derive

--------------------------------------------------------------------------------
--          Abs
--------------------------------------------------------------------------------

public export %hint
AbsToNumNP : NP (Abs . f) ks -> NP (Num . f) ks
AbsToNumNP = mapNP (\_ => materialize Num)

public export %hint
AbsToNumPOP : POP (Abs . f) kss -> POP (Num . f) kss
AbsToNumPOP = mapPOP (\_ => materialize Num)

-- Derivation of Abs for n-ary products or newtypes
-- e.g. `(a,b,c)` or `data Foo a = MkFoo a`  or  `data Foo = Foo Int`

-- n-ary products, e.g. (a,b,c)   (data Foo x y = MkFoo x y)
public export
NP (Abs . f) ks => Abs (NP f ks) where
  abs = hcliftA (Abs . f) abs

-- This type of deriving is for newtypes so we restrict oursellves to single constructor types.
public export
(all : NP (Abs . f) [k']) => Abs (NS f [k']) where
  abs {all = _::_} (Z x) = Z $ abs x

public export
POP (Abs . f) kss => Abs (POP f kss) where
  abs = hcliftA (Abs . f) abs

public export
POP (Abs . f) [ks] => Abs (SOP_ k f [ks]) where
  abs (MkSOP n) = MkSOP $ abs n

export
genericAbs : Generic t [code] => POP Abs [code] => t -> t
genericAbs = to . abs . from

export
||| Derives a `Abs` implementation for the given data type
||| and visibility.
AbsVis : Visibility -> DeriveUtil -> InterfaceImpl
AbsVis vis g = MkInterfaceImpl "Abs" vis []
                       `(MkAbs genericAbs)
                       (implementationType `(Abs) g)

export
||| Alias for `EncodeVis Public`.
Abs : DeriveUtil -> InterfaceImpl
Abs = AbsVis Public
