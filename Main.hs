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

-- TODO: universal quantification
-- TODO: proper subtyping
-- TODO: algebraic datatypes

-- TODO: fix possible runtime errors.  they are regarding universal quantification.

-- Primitive types correspond directly to the typedefs in C file (except for RF, CF).
data P = I32 | I64 | F32 | F64 | AI32 | AI64 | AF32 | AF64
       | O deriving (Show,Eq,Enum)
data RT = RI | RF32 | RFUN deriving (Show,Eq)

data TG = TG { name :: String, supertypes :: [T], subtypes :: [T], aliases :: [T] } deriving (Show,Eq)
data V = V String T deriving (Show,Eq)
data T = P String | Var String | SF (Stack T) (Stack T) | Forall [V] T
       | Error | U deriving (Show,Eq)
data L = CL String T | Err String | RL String RT deriving (Show,Eq)

--Type `Any' is considered to have all types as subtypes and none as supertypes.
types_init = [TG "Any" [] [] [], TG "Num" [] [P "I32"] [P "F32"]
             ,TG "Int" [P "Num"] [] [], TG "F32" [P "Num"] [] []]
funs_init = [CL "drop" (Forall [V "a" (P "Any")] (SF (Stack [Var "a"] 1) (Stack [] 0)))
            ,CL "swap" (SF (Stack [P "Any", P "Any"] 2) (Stack [P "Any",P "Any"] 2))]
finit_defs = [("drop",(\(Stack (s:xs) sz) -> Stack xs (sz-1)))
             ,("swap",(\(Stack (sa:sb:xs) sz) -> Stack (sb:sa:xs) sz))]

make_type :: DT.Text -> L
make_type l = case (readMaybe (DT.unpack l) :: Maybe Int, readMaybe (DT.unpack l) :: Maybe Float) of
  (Just a,_) -> CL (DT.unpack l) (P "I32")
  (_,Just a) -> CL (DT.unpack l) (P "F32")
  _          -> case find (\(CL n _) -> DT.unpack l==n) funs_init of
                  Just b -> b
                  _ -> CL (DT.unpack l) (U)

make_types :: [DT.Text] -> [L]
make_types = map make_type

-- NOTE: probably better to include Forall in type-checking.
parse :: [L] -> Stack L
parse = foldl (\st@(Stack s sz) cl@(CL ll lt) ->
  case lt of
    sa@(SF a b) -> exec_fun ll $ type_check st sa
    f@(Forall vs sf) -> exec_fun ll $ type_check st f
    _           -> push cl st) st_init

-- TODO: improve type mismatch error.
type_check :: Stack L -> T -> Stack L
type_check st@(Stack a n) (SF (Stack b _) _) =
  if length a >= length b && (all (\(CL ll lt,t) -> lt `is_subtype` t) $ zip a b)
  then st else Stack ((Err "type mismatch."):a) (n+1)
type_check st f = type_check st $ univ_quantify f

univ_quantify :: T -> T
univ_quantify (Forall vs t) = univ_quantify' vs t

univ_quantify' :: [V] -> T -> T
univ_quantify' vs (Var v) = case find (\(V a t) -> a==v) vs of
  Just (V _ t) -> t
  Nothing      -> Error
-- TODO: make only one recursive call.
univ_quantify' vs (SF (Stack a na) (Stack b nb)) =
  SF (Stack (map (univ_quantify' vs) a) na) (Stack (map (univ_quantify' vs) b) nb)
univ_quantify' vs (Forall vs' t) = univ_quantify $ Forall (vs++vs') t
univ_quantify' vs p = p

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
