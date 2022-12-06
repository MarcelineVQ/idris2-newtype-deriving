module Generics.Newtype.FromDouble

import Generics.Derive

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
FromDoubleVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
FromDoubleVis v _ p =
  let nm := implName p "FromDouble"
      cl := var nm .= `(MkFromDouble genericFromDouble)
   in Right [TL (interfaceHint v nm (implType "FromDouble" p)) (def nm [cl])]

export
||| Alias for `FromDouble Public`.
FromDouble : List Name -> ParamTypeInfo -> Res (List TopLevel)
FromDouble = FromDoubleVis Public
