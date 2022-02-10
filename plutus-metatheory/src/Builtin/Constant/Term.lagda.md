```
open import Builtin.Constant.Type
open import Utils hiding (TermCon)
```

```
module Builtin.Constant.Term
  (Ctx⋆ : Set)
  (_⊢⋆_ : Ctx⋆ → Kind → Set)
  (^ : ∀{Φ} → TyCon ♯ → Φ ⊢⋆ ♯)
  where


open import Builtin

open import Data.Integer
open import Data.String
open import Data.Bool
```

## Term Constants

```
data TermCon {Φ} : Φ ⊢⋆ ♯ → Set where
  integer    :
      (i : ℤ)
    → TermCon (^ integer)
  bytestring :
      (b : ByteString)
    → TermCon (^ bytestring)
  string     :
      (s : String)
    → TermCon (^ string)
  bool       :
      (b : Bool)
    → TermCon (^ bool)
  unit       : TermCon (^ unit)
  Data       : DATA → TermCon (^ Data)
```
