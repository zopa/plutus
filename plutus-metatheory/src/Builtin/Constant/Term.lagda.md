```
```

```
module Builtin.Constant.Term
  (Kind : Set)
  (♯ : Kind)
  (_⇒_ : Kind → Kind → Kind)
  where

open import Builtin.Constant.Type Kind ♯ _⇒_
open import Builtin

open import Data.Integer
open import Data.String
open import Data.Bool
open import Utils using (ByteString;DATA)
```

## Term Constants

```
data TermCon : TyCon ♯ → Set where
  integer    :
      (i : ℤ)
    → TermCon integer
  bytestring :
      (b : ByteString)
    → TermCon bytestring
  string     :
      (s : String)
    → TermCon string
  bool       :
      (b : Bool)
    → TermCon bool
  unit       : TermCon unit
  Data       : DATA → TermCon Data
```
