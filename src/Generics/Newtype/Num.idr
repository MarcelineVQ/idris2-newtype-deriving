module Generics.Newtype.Num

import Generics.Derive

--------------------------------------------------------------------------------
--          Num
--------------------------------------------------------------------------------

-- Derivation of Num for n-ary products or newtypes
-- e.g. `(a,b,c)` or `data Foo a = MkFoo a`  or  `data Foo = Foo Int`

-- n-ary products, e.g. (a,b,c)   (data Foo x y = MkFoo x y)
public export
(all : NP (Num . f) ks) => Num (NP f ks) where
  (+) = hcliftA2 (Num . f) (+)
  (*) = hcliftA2 (Num . f) (*)
  fromInteger {all = []} _ = []
  fromInteger {all = _::_} i = fromInteger i :: fromInteger i

-- This type of deriving is for newtypes so we restrict oursellves to single constructor types.
public export
(all : NP (Num . f) [k']) => Num (NS f [k']) where
  -- encode = hconcat . hcmap (Encode . f) encode
  (+) {all = _::_} (Z x) (Z y) = Z $ x + y
  (*) {all = _::_} (Z x) (Z y) = Z $ x * y
  fromInteger {all = _::_} i = Z (fromInteger i)

public export
POP (Num . f) kss => Num (POP f kss) where
  (+) (MkPOP nps1) (MkPOP nps2) = MkPOP $ nps1 + nps2
  (*) (MkPOP nps1) (MkPOP nps2) = MkPOP $ nps1 * nps2
  fromInteger i = MkPOP (fromInteger i)

public export
POP (Num . f) [ks] => Num (SOP f [ks]) where
  (+) (MkSOP nps1) (MkSOP nps2) = MkSOP $ nps1 + nps2
  (*) (MkSOP nps1) (MkSOP nps2) = MkSOP $ nps1 * nps2
  fromInteger i = MkSOP (fromInteger i)

export
genericPlus : Generic t [code] => POP Num [code] => t -> t -> t
genericPlus x y = to $ (+) (from x) (from y)

export
genericMult : Generic t [code] => POP Num [code] => t -> t -> t
genericMult x y = to $ (*) (from x) (from y)

export
genericFromInteger : Generic t [code] => POP Num [code] => Integer -> t
genericFromInteger = to . fromInteger

export
||| Derives a `Num` implementation for the given data type
||| and visibility.
NumVis : Visibility -> DeriveUtil -> InterfaceImpl
NumVis vis g = MkInterfaceImpl "Num" vis []
                       `(MkNum genericPlus genericMult genericFromInteger)
                       (implementationType `(Num) g)

export
||| Alias for `EncodeVis Public`.
Num : DeriveUtil -> InterfaceImpl
Num = NumVis Public
