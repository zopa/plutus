```
open import Builtin.Constant.Type
open import Utils hiding (TermCon)
```

```
module Builtin.Constant.Term
  (Ctx⋆ Kind : Set)
  (♯ * : Kind)
  (_⊢⋆_ : Ctx⋆ → Kind → Set)
  (^ : ∀{Φ} → TyCon Ctx⋆ (_⊢⋆ ♯) Φ → Φ ⊢⋆ ♯)
  (con : ∀{Φ} → Φ ⊢⋆ ♯ → Φ ⊢⋆ *)
  (Ctx : Ctx⋆ → Set)
  (_⊢_ : ∀{Φ}(Γ : Ctx Φ) → Φ ⊢⋆ * → Set)
  where


open import Builtin

open import Data.Integer
open import Data.String
open import Data.Bool
```

## Term Constants

```
data TermCon {Φ}(Γ : Ctx Φ) : Φ ⊢⋆ ♯ → Set where
  integer    :
      (i : ℤ)
    → TermCon Γ (^ integer)
  bytestring :
      (b : ByteString)
    → TermCon Γ (^ bytestring)
  string     :
      (s : String)
    → TermCon Γ (^ string)
  bool       :
      (b : Bool)
    → TermCon Γ (^ bool)
  unit       : TermCon Γ (^ unit)
  Data       : DATA → TermCon Γ (^ Data)
  nil        : ∀{a} → TermCon Γ (^ (list a))
  cons'      : ∀{a} → Γ ⊢ con a → TermCon Γ (^ (list a)) → TermCon Γ (^ (list a))
```
