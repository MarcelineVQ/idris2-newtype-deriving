module Generics.Newtype

import public Generics.Newtype.Num
import public Generics.Newtype.Neg
import public Generics.Newtype.Abs
import public Generics.Newtype.Fractional
import public Generics.Newtype.FromDouble
import public Generics.Newtype.Integral
import public Generics.Derive

-- %language ElabReflection
-- 
-- public export
-- data Raf = MkRaf Double
-- %runElab derive "Raf"
--   [Generic, Eq, Ord, Num, Neg, Abs, Fractional, FromDouble]
-- 
-- public export
-- data Raff a b = MkRaff a b
-- %runElab derive "Raff"
--   [Generic, Eq, Ord, Num, Neg, Abs, Fractional, FromDouble, Integral]
