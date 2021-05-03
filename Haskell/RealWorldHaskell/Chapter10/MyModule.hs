import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64)

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int64
} deriving (Show)

-- A Parse type, which is a wrapper around a parser function that
-- takes a parse state and returns either a string, or a tuple of some
-- baked in "a" value, and a new parse state
newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

-- An "identity" parser that bakes in an "a" value, yielding a lambda that
-- always returns that value, paired with whatever input state is passed in
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

-- Package up a bytestring "initial state" into a parse state, run a parser
-- on the parse state, then unpackage and return the result
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
    case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result

-- Could now execute the identity parser as follows:
-- > parse (identity 123) (L8.pack "init state (ignored)")
-- To give the result "Right 123"