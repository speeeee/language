import Data.List
import qualified Data.Text.IO as TIO
import qualified Data.Text as DT
import qualified Text.Show as SH
import Text.Read
import Control.Monad
import System.IO

data Stack a = Stack [a] Int deriving (Show,Eq)
push :: a -> Stack a -> Stack a
pop  :: Stack a -> Maybe (Stack a)
peek :: Stack a -> Maybe a
st_init :: Stack a

st_init = Stack [] 0
push x (Stack a n) = Stack (x:a) (n+1)
pop (Stack (s:xs) n) = Just (Stack xs (n-1))
pop (Stack [] _) = Nothing
peek (Stack (s:_) _) = Just s
peek (Stack [] _)  = Nothing

-- Primitive types correspond directly to the typedefs in C file (except for RF, CF).
data P = I32 | I64 | F32 | F64 | AI32 | AI64 | AF32 | AF64
       | O deriving (Show,Eq,Enum)
data RT = RI | RF32 | RFUN deriving (Show,Eq)

data TG = TG { name :: String, supertypes :: [T], subtypes :: [T], aliases :: [T] } deriving (Show,Eq)
data V = V String T deriving (Show,Eq)
data T = P String | Var String | SF (Stack T) (Stack T) | Forall [V] T | U deriving (Show,Eq)
data L = CL String T | Err String | RL String RT deriving (Show,Eq)

--Type `Any' is considered to have all types as subtypes and none as supertypes.
types_init = [TG "Any" [] [] [], TG "Num" [] [P "I32"] [P "F32"]
             ,TG "Int" [P "Num"] [] [], TG "F32" [P "Num"] [] []]
funs_init = [CL "pop" (SF (Stack [P "Any"] 1) (Stack [] 0))]
finit_defs = [("pop",(\(Stack (s:xs) sz) -> Stack xs (sz-1)))]

make_type :: DT.Text -> L
make_type l = case (readMaybe (DT.unpack l) :: Maybe Int, readMaybe (DT.unpack l) :: Maybe Float) of
  (Just a,_) -> CL (DT.unpack l) (P "I32")
  (_,Just a) -> CL (DT.unpack l) (P "F32")
  _          -> case find (\(CL n _) -> DT.unpack l==n) funs_init of
                  Just b -> b
                  _ -> CL (DT.unpack l) (U)

make_types :: [DT.Text] -> [L]
make_types = map make_type

parse :: [L] -> Stack L
parse = foldl (\st@(Stack s sz) cl@(CL ll lt) ->
  case lt of
    SF a b -> exec_fun ll $ type_check st a
    _      -> push cl st) st_init

-- TODO: improve type mismatch error.
type_check :: Stack L -> Stack T -> Stack L
type_check st@(Stack a n) (Stack b _) =
  if all (\(CL ll lt,t) -> lt `is_subtype` t) $ zip a b
  then st else Stack ((Err "type mismatch."):a) (n+1)

is_subtype :: T -> T -> Bool
-- TODO: find way to do commutativity.
is_subtype _ (P "Any") = True
is_subtype (P "Any") _ = True
-- Assume single parent for now.  ignore subtypes for now.
is_subtype (P a) (P b) = a==b

exec_fun :: String -> Stack L -> Stack L
exec_fun f s = case find ((==f) . fst) finit_defs of
  Just (_,fun) -> fun s
  Nothing      -> push (Err "not possible.") s

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  show <$> (parse <$> make_types <$> DT.words <$> TIO.getLine) >>= putStrLn
