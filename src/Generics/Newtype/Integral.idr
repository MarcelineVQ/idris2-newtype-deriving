module Generics.Newtype.Integral

import Generics.Newtype.Num

import Generics.Derive

--------------------------------------------------------------------------------
--          Integral
--------------------------------------------------------------------------------

public export %hint
IntegralToNumNP : NP (Integral . f) ks -> NP (Num . f) ks
IntegralToNumNP = mapNP (\_ => materialize Num)

public export %hint
IntegralToNumPOP : POP (Integral . f) kss -> POP (Num . f) kss
IntegralToNumPOP = mapPOP (\_ => materialize Num)

-- Derivation of Integral for n-ary products or newtypes
-- e.g. `(a,b,c)` or `data Foo a = MkFoo a`  or  `data Foo = Foo Int`

-- n-ary products, e.g. (a,b,c)   (data Foo x y = MkFoo x y)
public export
NP (Integral . f) ks => Integral (NP f ks) where
  div = hcliftA2 (Integral . f) div
  mod = hcliftA2 (Integral . f) mod

-- This type of deriving is for newtypes so we restrict oursellves to single constructor types.
public export
(all : NP (Integral . f) [k']) => Integral (NS f [k']) where
  div {all = _::_} (Z x) (Z y) = Z $ div x y
  mod {all = _::_} (Z x) (Z y) = Z $ mod x y

public export
POP (Integral . f) kss => Integral (POP f kss) where
  div = hcliftA2 (Integral . f) div
  mod = hcliftA2 (Integral . f) mod

public export
POP (Integral . f) [ks] => Integral (SOP_ k f [ks]) where
  MkSOP x `div` MkSOP y = MkSOP $ div x y
  MkSOP x `mod` MkSOP y = MkSOP $ mod x y

export
genericDivIntegral : Generic t [code] => POP Integral [code] => t -> t -> t
genericDivIntegral x y = to $ div (from x) (from y)

export
genericMod : Generic t [code] => POP Integral [code] => t -> t -> t
genericMod x y = to $ mod (from x) (from y)

export
||| Derives a `Integral` implementation for the given data type
||| and visibility.
IntegralVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
IntegralVis v _ p =
  let nm := implName p "Integral"
      cl := var nm .= `(MkIntegral genericDivIntegral genericMod)
   in Right [TL (interfaceHint v nm (implType "Integral" p)) (def nm [cl])]

export
||| Alias for `IntegralVis Public`.
Integral : List Name -> ParamTypeInfo -> Res (List TopLevel)
Integral = IntegralVis Public
