module TypeStack (Stack(..),T(..),LState(..),is_subtype,make_types,parse,types_init,funs_init,st_init) where

import Data.List
import qualified Data.Text.IO as TIO
import qualified Data.Text as DT
import qualified Text.Show as SH
import Text.Read
import Control.Monad
import System.IO

import Debug.Trace

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

-- TODO: functions that will be compiled rather than run immediately (needs type checking for
--     :   stack function result)
-- TODO: proper subtyping
-- TODO: algebraic datatypes

-- TODO: fix possible runtime errors.  they are regarding universal quantification.

-- TODO: type_sig and function definition.

-- Primitive types correspond directly to the typedefs in C file (except for RF, CF).
data P = I32 | I64 | F32 | F64 | AI32 | AI64 | AF32 | AF64
       | O deriving (Show,Eq,Enum)
data RT = RI | RF32 | RFUN deriving (Show,Eq)

data TG = TG { name :: T, supertypes :: [T], subtypes :: [T], aliases :: [T] } deriving (Show,Eq)
data V = V String T deriving (Show,Eq)
data T = P String | Var String | SF (Stack T) (Stack T) | Forall [V] T
       | Sum [T] | Product [T] | Error | U deriving (Show,Eq)
data L = CL String T | CT T | Err String | RL String RT deriving (Show,Eq)

data LState = LS { type_graph :: [TG], funs :: [L], stk :: Stack L }
  deriving (Show,Eq)

--Type `Any' is considered to have all types as subtypes and none as supertypes.
types_init = [TG (P "Any") [] [] [], TG (P "Sym") [] [] [], TG (P "Num") [] [P "I32"] [P "F32"]
             ,TG (P "I32") [P "Num"] [] [], TG (P "F32") [P "Num"] [] []]
funs_init = [CL "drop" (Forall [V "a" (P "Any")] (SF (Stack [Var "a"] 1) (Stack [] 0)))
            ,CL "swap" (SF (Stack [P "Any", P "Any"] 2) (Stack [P "Any",P "Any"] 2))
            ,CL "add" (Forall [V "a" (P "Num")] (SF (Stack [Var "a", Var "a"] 2) (Stack [Var "a"] 1)))
            ,CL "cons_sum" (SF (Stack [P "Any", P "Any"] 2) (Stack [P "Any"] 1))
            ,CL "cons_prod" (SF (Stack [P "Any", P "Any"] 2) (Stack [P "Any"] 1))]
finit_defs = [("drop",(\_ (Stack (s:xs) sz) -> Stack xs (sz-1)))
             ,("swap",(\_ (Stack (sa:sb:xs) sz) -> Stack (sb:sa:xs) sz))
             ,("add",(\_ s@(Stack (sa:sb:xs) sz) -> case (sa,sb) of
                         (CL a _,CL b _) -> Stack (CL (a++"+"++b) (P "I32"):xs) (sz-1)
                         _               -> s))
             ,("cons_sum",cons_sum),("cons_prod",cons_prod)]

cons_sum, cons_prod :: LState -> Stack L -> Stack L
cons_sum _ s@(Stack (a:b:cs) n) = case (a,b) of
  ((CT t1),(CT (Sum ts))) -> Stack (CT (Sum $ t1:ts):cs) (n-1)
  ((CT t1),(CT t2)) -> Stack (CT (Sum [t1,t2]):cs) (n-1)
cons_prod _ s@(Stack (a:b:cs) n) = case (a,b) of
  ((CT t1),(CT (Product ts))) -> Stack (CT (Product $ t1:ts):cs) (n-1)
  ((CT t1),(CT t2)) -> Stack (CT (Product [t1,t2]):cs) (n-1)

make_type :: DT.Text -> L
make_type l = case (readMaybe (DT.unpack l) :: Maybe Int, readMaybe (DT.unpack l) :: Maybe Float) of
  (Just a,_) -> CL (DT.unpack l) (P "I32")
  (_,Just a) -> CL (DT.unpack l) (P "F32")
  _          -> case find (\(CL n _) -> DT.unpack l==n) funs_init of
                  Just b -> b
                  _ -> CL (DT.unpack l) (U)

make_types :: [DT.Text] -> [L]
make_types = map make_type

-- TODO: merge lexing and parsing so that newly defined functions are lexed correctly on the
--     : first pass.
parse :: LState -> [L] -> LState
parse ls@(LS tg fs st) = LS tg fs . foldl (\st@(Stack s sz) cl@(CL ll lt) ->
  case lt of
    sa@(SF a b) -> exec_fun ls ll $ type_check tg st sa
    f@(Forall vs sf) -> exec_fun ls ll $ type_check tg st f
    _           -> push cl st) st

-- TODO: improve type mismatch error.
type_check :: [TG] -> Stack L -> T -> Stack L
type_check tg st@(Stack a n) (SF (Stack b _) _) =
  if length a >= length b && (all (\(CL ll lt,t) -> is_subtype tg lt t) $ zip a b)
  then st else Stack ((Err "type mismatch."):a) (n+1)
-- TODO: make pointfree, somehow not possible.
type_check tg st f@(Forall _ _) = univ_check tg st f

univ_check :: [TG] -> Stack L -> T -> Stack L
univ_check tg st (Forall vs t) = univ_check' tg st vs t

univ_check' :: [TG] -> Stack L -> [V] -> T -> Stack L
univ_check' tg st@(Stack a _) vs (SF (Stack b _) (Stack c _)) =
  let vs' = foldr (\(ka,vb) vss -> check1 tg ka (trace ("A:"++show vss) vss) vb) vs $ zip a b
  in if length a < length b then push (Err "stack underflow.") st
     else if any (\(V n _) -> n=="??") vs' then push (Err "type mismatch.") st
     else st

-- TODO: better error handling
check1 :: [TG] -> L -> [V] -> T -> [V]
check1 tg (CL a t') vs (Var v) = trace ("B:"++show (CL a t')++show vs++show (Var v)) $
  map (\(V n t) -> if n==v then if is_subtype tg t' t then V n t' else V "??" t else V n t) vs

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

-- TODO: write 'is_subtype' for other cases (e.g. Forall, SF)
is_subtype :: [TG] -> T -> T -> Bool
-- TODO: find way to do commutativity.
is_subtype _ _ (P "Any") = True
is_subtype _ (P "Any") _ = False
is_subtype tg pa@(P a) pb@(P b) = a==b
  || maybe False (any (\k -> is_subtype tg k pb)) (let a = supertypes <$> find (\q -> name q==pa) tg in trace (show a) a)

exec_fun :: LState -> String -> Stack L -> Stack L
exec_fun ls f s = case find ((==f) . fst) finit_defs of
  Just (_,fun) -> fun ls s
  Nothing      -> push (Err "not possible.") s
