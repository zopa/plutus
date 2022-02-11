```
open import Builtin.Constant.Type
```

```
module Builtin.Constant.Term
  (Ctx⋆ : Set)
  (Kind : Set)
  (♯ : Kind)
  (_⇒_ : Kind → Kind → Kind)
  (_⊢⋆_ : Ctx⋆ → Kind → Set)
  (^ : ∀{Φ} → TyCon Kind ♯ _⇒_ ♯ → Φ ⊢⋆ ♯)
  where


open import Builtin

open import Data.Integer
open import Data.String
open import Data.Bool
open import Utils using (ByteString;DATA)
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
