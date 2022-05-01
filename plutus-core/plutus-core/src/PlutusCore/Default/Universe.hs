{-# LANGUAGE StandaloneKindSignatures #-}
-- | The universe used by default and its instances.

{-# OPTIONS -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

-- effectfully: to the best of my experimentation, -O2 here improves performance, however by
-- inspecting GHC Core I was only able to see a difference in how the 'KnownTypeIn' instance for
-- 'Int' is compiled (one more call is inlined with -O2). This needs to be investigated.
{-# OPTIONS_GHC -O2 #-}

module PlutusCore.Default.Universe
    ( Single (..)
    , DefaultUni (..)
    , pattern DefaultUniList
    , pattern DefaultUniPair
    , module Export  -- Re-exporting universes infrastructure for convenience.
    ) where

import PlutusCore.Builtin
import PlutusCore.Data
import PlutusCore.Evaluation.Machine.Exception
import PlutusCore.Evaluation.Result

import Control.Applicative
import Data.ByteString qualified as BS
import Data.Int
import Data.IntCast (intCastEq)
import Data.Proxy
import Data.Text qualified as Text
import GHC.Exts (inline, oneShot)
import Universe as Export

import Data.Kind qualified as GHC
import GHC.Generics

{- Note [PLC types and universes]
We encode built-in types in PLC as tags for Haskell types (the latter are also called meta-types),
see Note [Universes]. A built-in type in PLC is an inhabitant of

    Some (TypeIn uni)

where @uni@ is some universe, i.e. a collection of tags that have meta-types associated with them.

A value of a built-in type is a regular Haskell value stored in

    Some (ValueOf uni)

(together with the tag associated with its type) and such a value is also called a meta-constant.

The default universe has the following constructor (pattern synonym actually):

    DefaultUniList :: !(DefaultUni a) -> DefaultUni [a]

But note that this doesn't directly lead to interop between Plutus Core and Haskell, i.e. you can't
have a meta-list whose elements are of a PLC type. You can only have a meta-list constant with
elements of a meta-type (i.e. a type from the universe).

However it is possible to apply a built-in type to an arbitrary PLC/PIR type, since we can embed
a type of an arbitrary kind into 'Type' and then apply it via 'TyApp'. But we only use it to
get polymorphic built-in functions over polymorphic built-in types, since it's not possible
to juggle values of polymorphic built-in types instantiated with non-built-in types at runtime
(it's not even possible to represent such a value in the AST, even though it's possible to represent
such a 'Type').

Finally, it is not necessarily the case that we need to allow embedding PLC terms into meta-constants.
We already allow built-in functions with polymorphic types. There might be a way to utilize this
feature and have meta-constructors as built-in functions.
-}


data Kind ann
    = Type ann
    | KindArrow ann (Kind ann) (Kind ann)
    deriving stock (Eq, Show, Functor, Generic)

-- | A 'Type' assigned to expressions.
data Type tyname (uni :: GHC.Type -> GHC.Type -> GHC.Type) ann
    = TyVar ann tyname
    | TyFun ann (Type tyname uni ann) (Type tyname uni ann)
    | TyIFix ann (Type tyname uni ann) (Type tyname uni ann)
      -- ^ Fix-point type, for constructing self-recursive types
    | TyForall ann tyname (Kind ann) (Type tyname uni ann)
    | TyBuiltin ann (SomeTypeIn (uni ())) -- ^ Builtin type
    | TyLam ann tyname (Kind ann) (Type tyname uni ann)
    | TyApp ann (Type tyname uni ann) (Type tyname uni ann)
    deriving stock (Show, Functor, Generic)

data Term tyname name (uni :: GHC.Type -> GHC.Type -> GHC.Type) fun ann
    = Var ann name -- ^ a named variable
    | TyAbs ann tyname (Kind ann) (Term tyname name uni fun ann)
    | LamAbs ann name (Type tyname uni ann) (Term tyname name uni fun ann)
    | Apply ann (Term tyname name uni fun ann) (Term tyname name uni fun ann)
    | Constant ann (Some (ValueOf (uni ()))) -- ^ a constant term
    | Builtin ann fun
    | TyInst ann (Term tyname name uni fun ann) (Type tyname uni ann)
    | Unwrap ann (Term tyname name uni fun ann)
    | IWrap ann (Type tyname uni ann) (Type tyname uni ann) (Term tyname name uni fun ann)
    | Error ann (Type tyname uni ann)
    deriving stock (Show, Functor, Generic)

newtype Single val = Single
    { unSingle :: val
    }

-- See Note [Representing polymorphism].
-- | The universe used by default.
data DefaultUni val a where
    DefaultUniInteger    :: DefaultUni val (Esc Integer)
    DefaultUniByteString :: DefaultUni val (Esc BS.ByteString)
    DefaultUniString     :: DefaultUni val (Esc Text.Text)
    DefaultUniUnit       :: DefaultUni val (Esc ())
    DefaultUniBool       :: DefaultUni val (Esc Bool)
    DefaultUniProtoList  :: DefaultUni val (Esc [])
    DefaultUniProtoPair  :: DefaultUni val (Esc (,))
    DefaultUniApply      :: !(DefaultUni val (Esc f)) -> !(DefaultUni val (Esc a)) -> DefaultUni val (Esc (f a))
    DefaultUniData       :: DefaultUni val (Esc Data)
    DefaultUniSingle     :: DefaultUni val (Esc (Single val))

-- GHC infers crazy types for these two and the straightforward ones break pattern matching,
-- so we just leave GHC with its craziness.
pattern DefaultUniList uniA =
    DefaultUniProtoList `DefaultUniApply` uniA
pattern DefaultUniPair uniA uniB =
    DefaultUniProtoPair `DefaultUniApply` uniA `DefaultUniApply` uniB

-- deriveGEq ''DefaultUni
-- deriveGCompare ''DefaultUni

instance GEq (DefaultUni val) where geq = undefined
-- instance GCompare (DefaultUni val) where geq = undefined

-- | For pleasing the coverage checker.
noMoreTypeFunctions :: DefaultUni val (Esc (f :: a -> b -> c -> d)) -> any
noMoreTypeFunctions (f `DefaultUniApply` _) = noMoreTypeFunctions f

instance ToKind (DefaultUni val) where
    toSingKind DefaultUniInteger        = knownKind
    toSingKind DefaultUniByteString     = knownKind
    toSingKind DefaultUniString         = knownKind
    toSingKind DefaultUniUnit           = knownKind
    toSingKind DefaultUniBool           = knownKind
    toSingKind DefaultUniProtoList      = knownKind
    toSingKind DefaultUniProtoPair      = knownKind
    toSingKind (DefaultUniApply uniF _) = case toSingKind uniF of _ `SingKindArrow` cod -> cod
    toSingKind DefaultUniData           = knownKind
    toSingKind DefaultUniSingle         = knownKind

instance HasUniApply (DefaultUni val) where
    matchUniApply (DefaultUniApply f a) _ h = h f a
    matchUniApply _                     z _ = z

instance GShow (DefaultUni val) where gshowsPrec = showsPrec
instance Show (DefaultUni val a) where
    show DefaultUniInteger             = "integer"
    show DefaultUniByteString          = "bytestring"
    show DefaultUniString              = "string"
    show DefaultUniUnit                = "unit"
    show DefaultUniBool                = "bool"
    show DefaultUniProtoList           = "list"
    show DefaultUniProtoPair           = "pair"
    show (uniF `DefaultUniApply` uniB) = case uniF of
        DefaultUniProtoList                          -> concat ["list (", show uniB, ")"]
        DefaultUniProtoPair                          -> concat ["pair (", show uniB, ")"]
        DefaultUniProtoPair `DefaultUniApply` uniA   -> concat ["pair (", show uniA, ") (", show uniB, ")"]
        uniG `DefaultUniApply` _ `DefaultUniApply` _ -> noMoreTypeFunctions uniG
    show DefaultUniData = "data"
    show DefaultUniSingle = "single"

instance DefaultUni val `Contains` Integer       where knownUni = DefaultUniInteger
instance DefaultUni val `Contains` BS.ByteString where knownUni = DefaultUniByteString
instance DefaultUni val `Contains` Text.Text     where knownUni = DefaultUniString
instance DefaultUni val `Contains` ()            where knownUni = DefaultUniUnit
instance DefaultUni val `Contains` Bool          where knownUni = DefaultUniBool
instance DefaultUni val `Contains` []            where knownUni = DefaultUniProtoList
instance DefaultUni val `Contains` (,)           where knownUni = DefaultUniProtoPair
instance DefaultUni val `Contains` Data          where knownUni = DefaultUniData
instance DefaultUni val `Contains` Single val    where knownUni = DefaultUniSingle

instance DefaultUni val `Contains` a => DefaultUni val `Contains` ([] a) where
    knownUni = knownUni `DefaultUniApply` knownUni

instance DefaultUni val `Contains` a => DefaultUni val `Contains` ((,) a) where
    knownUni = knownUni `DefaultUniApply` knownUni

instance (DefaultUni val `Contains` a, DefaultUni val `Contains` b) =>
            DefaultUni val `Contains` ((,) a b) where
    knownUni = knownUni `DefaultUniApply` knownUni

-- instance (DefaultUni val `Contains` f, DefaultUni val `Contains` a) => DefaultUni val `Contains` f a where
--     knownUni = knownUni `DefaultUniApply` knownUni

instance KnownBuiltinTypeAst (DefaultUni val) Integer       => KnownTypeAst (DefaultUni val) Integer
instance KnownBuiltinTypeAst (DefaultUni val) BS.ByteString => KnownTypeAst (DefaultUni val) BS.ByteString
instance KnownBuiltinTypeAst (DefaultUni val) Text.Text     => KnownTypeAst (DefaultUni val) Text.Text
instance KnownBuiltinTypeAst (DefaultUni val) ()            => KnownTypeAst (DefaultUni val) ()
instance KnownBuiltinTypeAst (DefaultUni val) Bool          => KnownTypeAst (DefaultUni val) Bool
instance KnownBuiltinTypeAst (DefaultUni val) [a]           => KnownTypeAst (DefaultUni val) [a]
instance KnownBuiltinTypeAst (DefaultUni val) (a, b)        => KnownTypeAst (DefaultUni val) (a, b)
instance KnownBuiltinTypeAst (DefaultUni val) Data          => KnownTypeAst (DefaultUni val) Data
instance KnownBuiltinTypeAst (DefaultUni val) (Single val)  => KnownTypeAst (DefaultUni val) (Single val)

{- Note [Constraints of ReadKnownIn and MakeKnownIn instances]
For a monomorphic data type @X@ one only needs to add a @HasConstantIn (DefaultUni val) term@ constraint
in order to be able to provide a @ReadTypeIn (DefaultUni val) term X@ instance and the same applies to
'MakeKnownIn'.

For a polymorphic data type @Y@ in addition to the same @HasConstantIn (DefaultUni val) term@ constraint
one also needs to add @(DefaultUni val) `Contains` Y@, where @Y@ contains all of its type variables.

See the reference site of this Note for examples.

The difference is due to the fact that for any monomorphic type @X@ the @(DefaultUni val) `Contains` X@
constraint can be discharged statically, so we don't need it to provide the instance, while in
the polymorphic case whether @Y@ is in the universe or not depends on whether its type arguments are
in the universe or not, so the @(DefaultUni val) `Contains` Y@ constraint can't be discharged statically.

Could we still provide @(DefaultUni val) `Contains` X@ even though it's redundant? That works, but then
GHC does not attempt to discharge it statically and takes the type tag needed for unlifting from
the provided constraint rather than the global scope, which makes the code measurably slower.

Could we at least hide the discrepancy behind a type family? Unfortunately, that generates worse
Core as some things don't get inlined properly. Somehow GHC is not able to see through the type
family that it fully reduces anyway.

Finally, instead of writing @(DefaultUni val) `Contains` Y@ we could write @(DefaultUni val) `Contains` a@
for each argument type @a@ in @Y@ (because that implies @(DefaultUni val) `Contains` Y@), however GHC
creates a redundant @let@ in that case (@-fno-cse@ or some other technique for preventing GHC from
doing CSE may solve that problem). For now we do the simplest thing and just write
@(DefaultUni val) `Contains` Y@.

A call to 'geq' does not get inlined due to 'geq' being recursive. It's an obvious inefficiency and
one that can be fixed, see https://github.com/input-output-hk/plutus/pull/4462
It's some pretty annoying boilerplate and for now we've decided it's not worth it.
-}

-- See Note [Constraints of ReadKnownIn and MakeKnownIn instances].
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term Integer
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term BS.ByteString
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term Text.Text
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term ()
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term Bool
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term Data
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term (Single val)
instance (HasConstantIn (DefaultUni val) term, (DefaultUni val) `Contains` [a]) =>
    MakeKnownIn (DefaultUni val) term [a]
instance (HasConstantIn (DefaultUni val) term, (DefaultUni val) `Contains` (a, b)) =>
    MakeKnownIn (DefaultUni val) term (a, b)

-- See Note [Constraints of ReadKnownIn and MakeKnownIn instances].
instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term Integer
instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term BS.ByteString
instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term Text.Text
instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term ()
instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term Bool
instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term Data
instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term (Single val)
instance (HasConstantIn (DefaultUni val) term, (DefaultUni val) `Contains` [a]) =>
    ReadKnownIn (DefaultUni val) term [a]
instance (HasConstantIn (DefaultUni val) term, (DefaultUni val) `Contains` (a, b)) =>
    ReadKnownIn (DefaultUni val) term (a, b)

-- If this tells you an instance is missing, add it right above, following the pattern.
instance TestTypesFromTheUniverseAreAllKnown (DefaultUni val)

{- Note [Int as Integer]
Technically our universe only contains 'Integer', but many of the builtin functions that we would
like to use work over 'Int'.

This is inconvenient and also error-prone: dealing with a function that takes an 'Int' means carefully
downcasting the 'Integer', running the function, potentially upcasting at the end. And it's easy to get
wrong by e.g. blindly using 'fromInteger'.

Moreover, there is a latent risk here: if we *were* to build on a 32-bit platform, then programs which
use arguments between @maxBound :: Int32@ and @maxBound :: Int64@ would behave differently!

So, what to do? We adopt the following strategy:
- We allow lifting/unlifting 'Int64' via 'Integer', including a safe downcast in 'readKnown'.
- We allow lifting/unlifting 'Int' via 'Int64', converting between them using 'intCastEq'.

This has the effect of allowing the use of 'Int64' always, and 'Int' iff it is provably equal to
'Int64'. So we can use 'Int' conveniently, but only if it has predictable behaviour.

(An alternative would be to just add 'Int', but add 'IntCastEq Int Int64' as an instance constraint.
That would also work, this way just seemed a little more explicit, and avoids adding constraints,
which can sometimes interfere with optimization and inling.)

Doing this effectively bans builds on 32-bit systems, but that's fine, since we don't care about
supporting 32-bit systems anyway, and this way any attempts to build on them will fail fast.

Note: we couldn't fail the bounds check with 'AsUnliftingError', because an out-of-bounds error is not an
internal one -- it's a normal evaluation failure, but unlifting errors have this connotation of
being "internal".
-}

instance KnownTypeAst (DefaultUni val) Int64 where
    toTypeAst _ = toTypeAst $ Proxy @Integer

-- See Note [Int as Integer].
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term Int64 where
    makeKnown = makeKnown . toInteger
    {-# INLINE makeKnown #-}

instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term Int64 where
    readKnown term =
        -- See Note [Performance of KnownTypeIn instances].
        -- Funnily, we don't need 'inline' here, unlike in the default implementation of 'readKnown'
        -- (go figure why).
        inline readKnownConstant term >>= oneShot \(i :: Integer) ->
            -- We don't make use here of `toIntegralSized` because of performance considerations,
            -- see: https://gitlab.haskell.org/ghc/ghc/-/issues/19641
            -- OPTIMIZE: benchmark an alternative `integerToIntMaybe`, modified from 'ghc-bignum'
            if fromIntegral (minBound :: Int64) <= i && i <= fromIntegral (maxBound :: Int64)
                then pure $ fromIntegral i
                else throwing_ _EvaluationFailure
    {-# INLINE readKnown #-}

instance KnownTypeAst (DefaultUni val) Int where
    toTypeAst _ = toTypeAst $ Proxy @Integer

-- See Note [Int as Integer].
instance HasConstantIn (DefaultUni val) term => MakeKnownIn (DefaultUni val) term Int where
    -- This could safely just be toInteger, but this way is more explicit and it'll
    -- turn into the same thing anyway.
    makeKnown = makeKnown . intCastEq @Int @Int64
    {-# INLINE makeKnown #-}

instance HasConstantIn (DefaultUni val) term => ReadKnownIn (DefaultUni val) term Int where
    readKnown term = intCastEq @Int64 @Int <$> readKnown term
    {-# INLINE readKnown #-}

{- Note [Stable encoding of tags]
'encodeUni' and 'decodeUni' are used for serialisation and deserialisation of types from the
universe and we need serialised things to be extremely stable, hence the definitions of 'encodeUni'
and 'decodeUni' must be amended only in a backwards compatible manner.

See Note [Stable encoding of PLC]
-}

instance Closed (DefaultUni val) where
    type DefaultUni val `Everywhere` constr =
        ( constr `Permits` Integer
        , constr `Permits` BS.ByteString
        , constr `Permits` Text.Text
        , constr `Permits` ()
        , constr `Permits` Bool
        , constr `Permits` []
        , constr `Permits` (,)
        , constr `Permits` Data
        , constr `Permits` Single val
        )

    -- See Note [Stable encoding of tags].
    -- IF YOU'RE GETTING A WARNING HERE, DON'T FORGET TO AMEND 'withDecodedUni' RIGHT BELOW.
    encodeUni DefaultUniInteger           = [0]
    encodeUni DefaultUniByteString        = [1]
    encodeUni DefaultUniString            = [2]
    encodeUni DefaultUniUnit              = [3]
    encodeUni DefaultUniBool              = [4]
    encodeUni DefaultUniProtoList         = [5]
    encodeUni DefaultUniProtoPair         = [6]
    encodeUni (DefaultUniApply uniF uniA) = 7 : encodeUni uniF ++ encodeUni uniA
    encodeUni DefaultUniData              = [8]
    encodeUni DefaultUniSingle            = [9]

    -- See Note [Decoding universes].
    -- See Note [Stable encoding of tags].
    withDecodedUni k = peelUniTag >>= \case
        0 -> k DefaultUniInteger
        1 -> k DefaultUniByteString
        2 -> k DefaultUniString
        3 -> k DefaultUniUnit
        4 -> k DefaultUniBool
        5 -> k DefaultUniProtoList
        6 -> k DefaultUniProtoPair
        7 ->
            withDecodedUni @(DefaultUni val) $ \uniF ->
                withDecodedUni @(DefaultUni val) $ \uniA ->
                    withApplicable uniF uniA $
                        k $ uniF `DefaultUniApply` uniA
        8 -> k DefaultUniData
        _ -> empty

    bring
        :: forall constr a r proxy. DefaultUni val `Everywhere` constr
        => proxy constr -> DefaultUni val (Esc a) -> (constr a => r) -> r
    bring _ DefaultUniInteger    r = r
    bring _ DefaultUniByteString r = r
    bring _ DefaultUniString     r = r
    bring _ DefaultUniUnit       r = r
    bring _ DefaultUniBool       r = r
    bring p (DefaultUniProtoList `DefaultUniApply` uniA) r =
        bring p uniA r
    bring p (DefaultUniProtoPair `DefaultUniApply` uniA `DefaultUniApply` uniB) r =
        bring p uniA $ bring p uniB r
    bring _ (f `DefaultUniApply` _ `DefaultUniApply` _ `DefaultUniApply` _) _ =
        noMoreTypeFunctions f
    bring _ DefaultUniData r = r
    bring _ DefaultUniSingle r = r
