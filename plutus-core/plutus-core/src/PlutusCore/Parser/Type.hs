{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module PlutusCore.Parser.Type
    ( IdentifierState
    , newIdentifier
    , emptyIdentifierState
    , identifierStateFrom
    ) where

import PlutusPrelude (Generic, NFData, Natural, Pretty (pretty))

import PlutusCore.Name

import Control.Monad.State
import Data.Map qualified as M
import Data.Text qualified as T
import PlutusCore.Default.Builtins (DefaultFun)
import Prettyprinter.Internal ((<+>))
import Text.Megaparsec (SourcePos)

{- Note [Literal Constants]
For literal constants, we accept certain types of character sequences that are
then passed to user-defined parsers which convert them to built-in constants.
Literal constants have to specify the type of the constant, so we have (con
integer 9), (con string "Hello"), and so on.  This allows us to use the same
literal syntax for different types (eg, integer, short, etc) and shift most
of the responsibility for parsing constants out of the lexer and into the
parser (and eventually out of the parser to parsers supplied by the types
themselves).

In the body of a constant we allow:
  * ()
  * Single-quoted possibly empty sequences of printable characters
  * Double-quoted possibly empty sequences of printable characters
  * Unquoted non-empty sequences of printable characters not including '(' or ')',
    and not beginning with a single or double quote.  Spaces are allowed in the
    body of the sequence, but are ignored at the beginning or end.

"Printable" here uses Alex's definition: Unicode code points 32 to 0x10ffff.
This includes spaces but excludes tabs amongst other things.  One can use the
usual escape sequences though, as long as the type-specific parser deals with
them.

These allow us to parse all of the standard types.  We just return all of the
characters in a TkLiteralConst token, not attempting to do things like stripping
off quotes or interpreting escape sequences: it's the responsibility of the
parser for the relevant type to do these things.  Note that 'read' will often do
the right thing.

The final item above even allows the possibility of parsing complex types such as
tuples and lists as long as parentheses are not involved.  For example, (con
tuple <1,2.3,"yes">) and (con intlist [1, 2, -7]) are accepted by the lexer, as
is the somewhat improbable-looking (con intseq 12 4 55 -4).  Comment characters
are also allowed, but are not treated specially.  We don't allow (con )) or (con
tuple (1,2,3)) because it would be difficult for the lexer to decide when it
had reached the end of the literal: consider a tuple containing a quoted string
containing ')', for example.
-}

-- | A parser term. It includes terms and types and their arguments.
-- Some of these are only for UPLC or TPLC, but it's simplest to share
-- the parser, so we have a joint enumeration of them.
data ParserTerm
    = Program ParserTerm ParserTerm
    -- ^ A program is parameterised by a version number and a term
    | Version Natural Natural Natural
    -- ^ Version number for a program
    -- Terms for PLC, UPLC, PIR
    | LamAbs ParserTerm
    | Constant ParserTerm ParserTerm
    -- ^ A constant term parameterised by a builtin type (@TyBuiltin@)
    -- and the constant, e.g., @Integer@ or @LiteralConst@.
    | Integer Integer
    -- ^ An integer constant.
    | LiteralConst LiteralConst
    -- ^ A literal constant, may be (), unwrapped chars or
    -- chars wrapped in single or double quotes.
    | Builtin DefaultFun
    -- ^ Builtin functions in the default universe.
    -- Parsing of other universe's functions are not supported atm.
    | Error
    -- TPLC only
    | KwAbs
    | KwFun
    | KwAll
    | KwType
    | KwIFix
    | KwIWrap
    | KwUnwrap
    -- UPLC only
    | KwForce
    | KwDelay
    -- Types
    | TyBuiltin
    deriving (Show, Eq, Ord, Generic, NFData)

-- See note [Literal Constants].
-- | A literal constant.
data LiteralConst
    = EmptyBrackets
    -- ^ ()
    | SingleQuotedChars
    -- ^ Single-quoted possibly empty sequences of printable characters
    | DoubleQuotedChars
    -- ^ Double-quoted possibly empty sequences of printable characters
    | UnQuotedChars
    -- ^ Unquoted non-empty sequences of printable characters not including '(' or ')',
    -- and not beginning with a single or double quote.  Spaces are allowed in the
    -- body of the sequence, but are ignored at the beginning or end.
    deriving (Show, Eq, Ord, Generic, NFData)

instance Pretty LiteralConst where
    pretty EmptyBrackets     = "lit ()"
    pretty SingleQuotedChars = "lit '"
    pretty DoubleQuotedChars = "lit \""
    pretty UnQuotedChars     = "lit"

-- | An 'IdentifierState' includes a map indexed by 'Int's as well as a map
-- indexed by 'ByteString's. It is used during parsing.
type IdentifierState = (M.Map T.Text Unique, Unique)

emptyIdentifierState :: IdentifierState
emptyIdentifierState = (mempty, Unique 0)

identifierStateFrom :: Unique -> IdentifierState
identifierStateFrom u = (mempty, u)

newIdentifier :: (MonadState IdentifierState m) => T.Text -> m Unique
newIdentifier str = do
    (ss, nextU) <- get
    case M.lookup str ss of
        Just k -> pure k
        Nothing -> do
            let nextU' = Unique $ unUnique nextU + 1
            put (M.insert str nextU ss, nextU')
            pure nextU
