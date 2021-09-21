Newtype Generic Deriving
=====

This package provides sorely-missed deriving for common interfaces like `Num` for newtypes and newtype-likes, iow single-constructor types. For instance:
```idris
import Generics.Newtype
%language ElabReflection

data Raf = MkRaf Double
%runElab derive "Raf"
  [Generic, Eq, Ord, Num, Neg, Abs, Fractional, FromDouble]

data Raff a b = MkRaff a b
%runElab derive "Raff"
  [Generic, Eq, Ord, Num, Neg, Abs, Fractional, FromDouble, Integral]

```
Your projects using this lib need to depend on [sop](https://github.com/stefan-hoeck/idris2-sop) and [elab-util](https://github.com/stefan-hoeck/idris2-elab-util).

Code here is based on the pristine work done by [Stefan Hoeck](https://github.com/stefan-hoeck/idris2-sop).

This package could be improved by scanning interfaces and generating the implementations programmaticlly but it does not do so currently and the deriving machinery is instead assembled by hand.


Version
-------

This package follows [Haskell PVP](https://pvp.haskell.org/) which is distinct from [SEMVER](https://semver.org/) in that when examining `1.2.3`, `1.2`  is the Major Version rather than `1`.
