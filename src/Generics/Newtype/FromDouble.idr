module Generics.Newtype.FromDouble

import Generics.Derive
%language ElabReflection

--------------------------------------------------------------------------------
--          FromDouble
--------------------------------------------------------------------------------

-- Derivation of FromDouble for n-ary products or newtypes
-- e.g. `(a,b,c)` or `data Foo a = MkFoo a`  or  `data Foo = Foo Int`

-- n-ary products, e.g. (a,b,c)   (data Foo x y = MkFoo x y)
public export
(all : NP (FromDouble . f) ks) => FromDouble (NP f ks) where
  fromDouble {all = []} _ = []
  fromDouble {all = _::_} d = fromDouble d :: fromDouble d

-- This type of deriving is for newtypes so we restrict oursellves to single constructor types.
public export
(all : NP (FromDouble . f) [k']) => FromDouble (NS f [k']) where
  fromDouble {all = _::_} i = Z (fromDouble i)

public export
POP (FromDouble . f) kss => FromDouble (POP f kss) where
  fromDouble i = MkPOP (fromDouble i)

public export
POP (FromDouble . f) [ks] => FromDouble (SOP f [ks]) where
  fromDouble i = MkSOP (fromDouble i)

export
genericFromDouble : Generic t [code] => POP FromDouble [code] => Double -> t
genericFromDouble = to . fromDouble

export
||| Derives a `FromDouble` implementation for the given data type
||| and visibility.
FromDoubleVis : Visibility -> DeriveUtil -> InterfaceImpl
FromDoubleVis vis g = MkInterfaceImpl "FromDouble" vis []
                       `(MkFromDouble genericFromDouble)
                       (implementationType `(FromDouble) g)

export
||| Alias for `EncodeVis Public`.
FromDouble : DeriveUtil -> InterfaceImpl
FromDouble = FromDoubleVis Public
