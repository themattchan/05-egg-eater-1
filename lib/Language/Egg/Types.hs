{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Egg.Types
  (
  -- * Re-Export SourceSpans
    module Language.Egg.UX

  -- * Abstract syntax of (a small subset of) x86 assembly instructions
  , Instruction (..)
  , Arg (..)
  , Reg (..)
  , Size (..)

  -- * Aliases for various identifiers
  , Id
  , Tag

  -- * Abstract syntax of the Adder language
  , Program (..), BareProgram , AnfProgram
  , Decl (..)   , BareDecl    , AnfDecl
  , Bind (..)   , BareBind
  , Expr (..)   , Bare        , AnfExpr, ImmExpr
  , Prim1 (..)
  , Prim2 (..)
  , isAnf

  -- * Smart Constructors
  , bindsExpr
  , exprsExpr

  -- * Destructors
  , exprBinds
  , bindId

  -- * Labels
  , label
  , getLabel

    -- * Environments
  , Env
  , emptyEnv
  , pushEnv
  , lookupEnv
  , memberEnv
  , addEnv
  , fromListEnv
  , envMax
  -- , insertEnv

  -- * Dynamic Errors
  , DynError (..)
  , Ty (..)

  -- * Code Labels
  , Label (..)

  -- * Abstract Text Type
  , Ext (..)
  , ext

  ) where

import           Prelude
import qualified Data.List        as L
import           Data.Maybe                       (isJust)
import           Text.Printf
import           System.FilePath                  ((<.>))
import           Language.Egg.UX

import Control.Monad.State.Class
-- import Control.Monad.Writer.Class
-- import Control.Monad.Trans.Writer.Lazy (WriterT(..))
import Control.Monad.Trans.State.Lazy (evalState)

import qualified Data.Kind as GHC (Type)
--import GHC.Exts (Constriant)
data Reg
  = EAX
  | EBX
  | ESP
  | EBP
  | ESI

data Size
  = DWordPtr
  | WordPtr
  | BytePtr

data Arg
  = Const     Int
  | HexConst  Int
  | Reg            Reg
  | RegOffset Nat  Reg
  | RegIndex  Reg  Reg
  | Sized     Size Arg

type ListNE a = [a]
type Nat      = Int

-- | Control-Flow Labels (New)
data Label
  = BranchTrue Tag
  | BranchDone Tag
  | LamStart   Tag
  | LamEnd     Tag
  | DefStart   Id Tag
  | DefEnd     Id Tag
  | DynamicErr DynError
  | Builtin    Text
  deriving (Show)

-- | Machine (x86) Instructions
data Instruction
  = IMov    Arg   Arg
  | IAdd    Arg   Arg
  | ISub    Arg   Arg
  | IMul    Arg   Arg
  | IShr    Arg   Arg
  | ISar    Arg   Arg
  | IShl    Arg   Arg
  | IAnd    Arg   Arg
  | IOr     Arg   Arg
  | IXor    Arg   Arg
  | ILabel  Label
  | IPush   Arg
  | IPop    Arg
  | ICmp    Arg   Arg
  | IJe     Label
  | IJne    Label
  | IJg     Label
  | IJl     Label
  | IJo     Label
  | IJmp    Label
  | ICall   Label
  | IRet

--------------------------------------------------------------------------------
-- | Abstract syntax of the Adder language
--------------------------------------------------------------------------------

-- | `Id` are program variables
type Id = Text

-- | `Tag` are used to tag each `If`
type Tag = Int

-- | `Prim1` are unary operations
data Prim1
  = Add1
  | Sub1
  | Print
  | IsNum
  | IsBool
  | IsTuple
  deriving (Show)

-- | `Prim2` are binary operations
data Prim2
  = Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal
  deriving (Show)


-- | Phases of AST transformation
data Phase = Bare | Tagged | Anfed
  deriving (Eq, Ord, Show, Bounded, Enum)
--           | Anfed AnfExpType
--  this does not work because 'Anfed 'IsImm /= 'Anfed 'IsAnf
-- ... how can you use an index to determine a gadt???
-- want something like:

-- phase = Bare
-- Prim1 :: Prim1 -> Expr Bare () -> Annot Bare -> Expr Bare ()

-- phase = Anfed
-- Prim1 :: Prim1 -> Expr Anfed IsImm -> Annot Anfed -> Expr Anfed IsAnf


--Expr (p :: Phase) (r :: Refinement p)

-- | Phase-indexed annotations
type family Annot (p :: Phase) :: GHC.Type where
  Annot 'Bare = SourceSpan
  Annot 'Tagged = (SourceSpan, Int)
  Annot 'Anfed = (SourceSpan, Int)

-- | Index for subterms in ANF
data AnfExpType :: GHC.Type where
  IsImm :: AnfExpType
  IsAnf :: AnfExpType

-- | Phase-indexed kind of indices
type family Refinement (p :: Phase) :: GHC.Type where
  Refinement 'Anfed = AnfExpType
  Refinement 'Bare = ()
  Refinement 'Tagged = ()

-- | Index expr as Imm or Anf, after ANFification
type family ImmIfAnf  (p :: Phase) :: Refinement p where
  ImmIfAnf  'Anfed = 'IsImm
  ImmIfAnf  'Bare  = '()
  ImmIfAnf  'Tagged  = '()
type family AnfIfAnf  (p :: Phase) :: Refinement p where
  AnfIfAnf  'Anfed = 'IsAnf
  AnfIfAnf  'Bare  = '()
  AnfIfAnf  'Tagged  = '()

-- | Expr are single expressions
data Expr (p :: Phase) (r :: Refinement p) where

   Number  :: !Integer -> Annot p -> Expr p r
   Boolean :: !Bool    -> Annot p -> Expr p r
   Id      :: !Id      -> Annot p -> Expr p r

   Prim1   :: !Prim1
           -> !(Expr p (ImmIfAnf p))
           -> Annot p
           -> Expr p (AnfIfAnf p)

   Prim2   :: !Prim2
           -> !(Expr p (ImmIfAnf p))
           -> !(Expr p (ImmIfAnf p))
           -> Annot p
           -> Expr p (AnfIfAnf p)

   If      :: !(Expr p (ImmIfAnf p))
           -> !(Expr p (AnfIfAnf p))
           -> !(Expr p (AnfIfAnf p))
           -> Annot p
           -> Expr p (AnfIfAnf p)

   Let     :: !(Bind p)
           -> !(Expr p (AnfIfAnf p))
           -> !(Expr p (AnfIfAnf p))
           -> Annot p
           -> Expr p (AnfIfAnf p)

   App     :: !Id
           -> [Expr p (ImmIfAnf p)]
           -> Annot p
           -> Expr p (AnfIfAnf p)

   Tuple   :: [Expr p (ImmIfAnf p)]
           -> Annot p
           -> Expr p (AnfIfAnf p)

   GetItem :: !(Expr p (ImmIfAnf p))
           -> !(Expr p (ImmIfAnf p))
           -> Annot p
           -> Expr p (AnfIfAnf p)

deriving instance forall a b. Show (Expr a b)

---    deriving (Show, Functor)

-- | Bind represent the let- or function-params.

data Bind (p :: Phase)
  = Bind !Id (Annot p)
--    deriving (Show)
deriving instance forall a. Show (Bind a)

-- | Decl are function definitions
data Decl (p :: Phase) = Decl
  { fName  :: !(Bind p)
  , fArgs  :: [Bind p]
  , fBody  :: !(Expr p (AnfIfAnf p))
  , fLabel :: Annot p
  }
--  deriving (Functor)

deriving instance forall a. Show (Decl a)

{-@ data Decl <p :: Expr a -> Prop> a = Decl
      { fName  :: Bind a
      , fArgs  :: [Bind a]
      , fBody  :: (Expr a)<p>
      , fLabel :: a
      }
  @-}

-- | A Program is a list of declarations and "main" Expr
data Program (p :: Phase) = Prog
  { pDecls :: [Decl p]
  , pBody  :: !(Expr p (AnfIfAnf p))
  }
--  deriving (Functor)

deriving instance forall p. Show (Program p)

{-@ data Program <p :: Expr a -> Prop> a = Prog
      { pDecls :: [Decl<p> a]
      , pBody  :: (Expr a)<p>
      }
  @-}


bindId :: Bind a -> Id
bindId (Bind x _) = x

-- -- | Constructing `Expr` from a sequence of `Expr`
-- exprsExpr :: forall (p :: Phase) (r :: Refinement p). ListNE (Expr p r) -> Annot p -> Expr p (AnfIfAnf p)
-- exprsExpr [e] _ = e
-- exprsExpr es  l = Tuple es l


-- -- | Constructing `Expr` from let-binds
-- bindsExpr :: [(Bind a, Expr a r)] -> Expr a r -> Annot a -> Expr a r
-- bindsExpr bs e l = foldr (\(x, e1) e2  -> Let x e1 e2 l) e bs

-- -- | Destructing `Expr` into let-binds
-- exprBinds :: Expr a r -> ([(Bind a, Expr a r)], Expr a r)
-- exprBinds (Let x e e' _) = ((x, e) : bs, body)
--   where
--     (bs, body)           = exprBinds e'
-- exprBinds body           = ([]        , body)

--------------------------------------------------------------------------------
getLabel :: Expr a r -> Annot a
--------------------------------------------------------------------------------
getLabel (Number _ l)    = l
getLabel (Boolean _ l)   = l
getLabel (Id _ l)        = l
getLabel (Prim1 _ _ l)   = l
getLabel (Prim2 _ _ _ l) = l
getLabel (If    _ _ _ l) = l
getLabel (Let _ _ _ l)   = l
getLabel (App _ _ l)     = l
getLabel (Tuple _ l)     = l
getLabel (GetItem _ _ l) = l

-- TODO use lenses !!!

-- TODO abstract this to a constrained Functor instance
-- class CFunctor (kind :: GHC.Type) (functor :: kind -> GHC.Type -> GHC.Type) where
--   cfmap :: forall (a :: kind) (b :: kind) (c :: GHC.Type). (c a -> c b)

-- mapAnnotE :: forall (a :: Phase) (b :: Phase) . (Annot a -> Annot b) -> Expr a (r a) -> Expr b (r b)
-- mapAnnotE f (Number a l)    = (Number a <$> (f l))
-- mapAnnotE f (Boolean a l)   = (Boolean a <$> (f l))
-- mapAnnotE f (Id a l)        = (Id a <$> (f l))
-- mapAnnotE f (Prim1 a b l)   = (Prim1 a b <$> (f l))
-- mapAnnotE f (Prim2 a b c l) = (Prim2 a b c <$> (f l))
-- mapAnnotE f (If a b c l)    = (If a b c <$> (f l))
-- mapAnnotE f (Let a b c l)   = (Let a b c <$> (f l))
-- mapAnnotE f (App a b l)     = (App a b <$> (f l))
-- mapAnnotE f (Tuple a l)     = (Tuple a <$> (f l))
-- mapAnnotE f (GetItem a b l) = (GetItem a b <$> (f l))


--------------------------------------------------------------------------------
-- | Dynamic Errors
--------------------------------------------------------------------------------

-- | DynError correspond to different kind of dynamic/run-time errors
data DynError
  = TypeError Ty
  | ArithOverflow
  | IndexLow
  | IndexHigh
  deriving (Show)

data Ty
  = TNumber
  | TBoolean
  | TTuple
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | Pretty Printer
--------------------------------------------------------------------------------
instance PPrint Ty where
  pprint TNumber  = "number"
  pprint TTuple   = "tuple"
  pprint TBoolean = "boolean"

instance PPrint Prim1 where
  pprint Add1    = "add1"
  pprint Sub1    = "sub1"
  pprint Print   = "print"
  pprint IsNum   = "isNum"
  pprint IsBool  = "isBool"
  pprint IsTuple = "isTuple"

instance PPrint Prim2 where
  pprint Plus    = "+"
  pprint Minus   = "-"
  pprint Times   = "*"
  pprint Less    = "<"
  pprint Greater = ">"
  pprint Equal   = "=="

instance PPrint Bool where
  pprint True  = "true"
  pprint False = "false"

instance PPrint (Bind a) where
  pprint (Bind x _) = x

instance PPrint (Expr a r) where
  pprint (Number n _)    = show n
  pprint (Boolean b _)   = pprint b
  pprint (Id x _)        = x
  pprint (Prim1 o e _)   = printf "%s(%s)"               (pprint o)   (pprint e)
  pprint (Prim2 o l r _) = printf "%s %s %s"             (pprint l)   (pprint o) (pprint r)
  pprint (If    c t e _) = printf "(if %s: %s else: %s)" (pprint c)   (pprint t) (pprint e)
  pprint e@(Let {})      = printf "(let %s in %s)"       (ppBinds bs) (pprint b) where (bs, b) = exprBinds e
  pprint (App f es _)    = printf "%s(%s)"               f            (pprintMany es)
  pprint (Tuple es _)    = printf "(%s)"                 (pprintMany es)
  pprint (GetItem e i _) = printf "%s[%s]"               (pprint e)   (pprint i)

instance PPrint (Decl a) where
  pprint (Decl f xs e _) = printf "def %s(%s):\n%s" (pprint f) (pprintMany xs) body
    where
      body               = nest 4 (pprint e)

instance PPrint (Program a) where
  pprint (Prog ds e)     = L.intercalate "\n" (map pprint ds ++ [pprint e])

nest       :: Int -> Text -> Text
nest n     = unlines . map pad . lines
  where
    pad s  = blanks ++ s
    blanks = replicate n ' '

pprintMany :: (PPrint a) => [a] -> Text
pprintMany xs = L.intercalate ", " (map pprint xs)

ppBinds :: [(Bind a, Expr a r)] -> Text
ppBinds bs = L.intercalate ", " [ printf "%s = %s" (pprint x) (pprint v) | (x, v) <- bs ]


--------------------------------------------------------------------------------
-- | Transformation to ensure each sub-expression gets a distinct tag
--------------------------------------------------------------------------------
label :: Program 'Bare -> Program 'Tagged
label (Prog ds e) = evalState (Prog . labelD ds <$> labelE e') 1
  -- where
  --   (i', ds')     = L.mapAccumL labelD 1  ds
  --   (_ , e')      =             labelE i' e

--------------------------------------------------------------------------------
labelD :: MonadState Int m => Decl 'Bare -> m TaggedExpr
--------------------------------------------------------------------------------
labelD (Decl f xs e l) = Decl <$> labelBind f <*> mapM labelBind xs <*> labelE e <*> tagAnnot l
-- labelD i (Decl f xs e l) = labelTop i'' l (Decl f' xs' e')
--   where
--     (i', e')             = labelE i e
--     (i'', f':xs')        = L.mapAccumL labelBind i' (f:xs)

--------------------------------------------------------------------------------
labelE :: MonadState Int m => BareExpr -> m TaggedExpr
--------------------------------------------------------------------------------
labelE = go
  where
    go (Number n l)      = Number n <$> tagAnnot l

    go (Boolean b l)     = Boolean b <$> tagAnnot l

    go (Id     x l)      = Id x <$> tagAnnot l

    go (Prim1 o e1 l)    = Prim1 o <$> go e1 <*> tagAnnot l

    go (Prim2 o e1 e2 l) = Prim2 o <$> go e1 <*> go e2 <*> tagAnnot l

    go (If c e1 e2 l)    = If <$> go c <*> go e1 <*> go e2 <*> tagAnnot l

    go (Let x e b l)     = do
      e' <- go e
      b' <- go b
      x' <- labelBind x
      l' <- tagAnnot l
      pure (Let x' e' b' l)

    go (Tuple es l)      = Tuple <$> mapM go es <*> tagAnnot l

    go (GetItem e1 e2 l) = GetItem <$> go e1 <*> go e2 <*> tagAnnot l

    go (App f es l)      = App f <$> mapM go es <*> tagAnnot l

--mapAnnotE :: forall (a :: Phase) (b :: Phase). (Annot a -> Annot b) -> Expr a -> Expr b
-- labelTop :: MonadState Int m => BareExpr -> m TaggedExpr
-- labelTop = mapAnnotE tagAnnot
--labelTop i l c             = (i + 1, c (l, i))

labelBind :: MonadState Int m => BareBind -> m TaggedBind
labelBind (Bind x l) = Bind x <$> tagAnnot l

tagAnnot :: MonadState Int m => Annot 'Bare -> m (Annot 'Tagged)
tagAnnot annot0 = do
  i <- get
  put (i+1)
  return (annot0, i)

--------------------------------------------------------------------------------
-- | `isAnf e` is True if `e` is an A-Normal Form
--------------------------------------------------------------------------------
{-@ measure isAnf @-}
isAnf :: Expr a r -> Bool
isAnf (Number  _ _)    = True
isAnf (Boolean _ _)    = True
isAnf (Id      _ _)    = True
isAnf (Prim1 _ e _)    = isImm e
isAnf (Prim2 _ e e' _) = isImm e && isImm e'
isAnf (If c t e _)     = isImm c && isAnf t && isAnf e
isAnf (Let _ e e' _)   = isAnf e && isAnf e'
isAnf (Tuple es _)     = all isImm es
isAnf (GetItem e i _)  = isImm e && isImm i
isAnf (App _ es _)     = all isImm es

{-@ measure isImm @-}
isImm :: Expr a r -> Bool
isImm (Number  _ _) = True
isImm (Boolean _ _) = True
isImm (Id      _ _) = True
isImm _             = False

{-@ type AnfExpr a = {v:Expr a| isAnf v} @-}
type AnfExpr = Expr Anfed 'IsAnf

{-@ type ImmExpr a = {v:Expr a | isImm v} @-}
type ImmExpr = Expr 'Anfed 'IsImm

{-@ type AnfDecl a = Decl<{\e -> isAnf e}> a @-}
type AnfDecl    = Decl 'Anfed

{-@ type AnfProgram a = Program<{\e -> isAnf e}> @-}
type AnfProgram = Program 'Anfed

--------------------------------------------------------------------------------
-- | The `Bare` types are for parsed ASTs.
--------------------------------------------------------------------------------

type BareExpr = Expr 'Bare '()
type BareBind = Bind 'Bare

type BareProgram = Program 'Bare
type BareDecl    = Decl    'Bare

instance Located BareExpr where
  sourceSpan = getLabel

instance Located BareBind where
  sourceSpan (Bind _ l) = l

type TaggedExpr = Expr 'Tagged '()
type TaggedBind = Bind 'Tagged

--------------------------------------------------------------------------------
-- | Functions for accessing the "environment" (stack)
--------------------------------------------------------------------------------

-- | An `Env` is a lookup-table mapping `Id` to some Int value
data Env = Env { envBinds :: [(Id, Int)]
               , envMax   :: !Int
               }
           deriving (Show)

emptyEnv :: Env
emptyEnv = Env [] 0

lookupEnv :: Id -> Env -> Maybe Int
lookupEnv k env = lookup k (envBinds env)

memberEnv :: Id -> Env -> Bool
memberEnv k env = isJust (lookupEnv k env)

pushEnv :: Bind a -> Env -> (Int, Env)
pushEnv x (Env bs n) = (n', Env bs' n')
  where
    bs'              = (bindId x, n') : bs
    n'               = 1 + n

addEnv :: Bind a -> Env -> Env
addEnv x env = snd (pushEnv x env)

fromListEnv :: [(Id, Int)] -> Env
fromListEnv bs = Env bs n
  where
    n          = maximum (0 : [i | (_, i) <- bs])

--------------------------------------------------------------------------------
-- | File Extensions
--------------------------------------------------------------------------------

data Ext = Src    -- ^ source
         | Asm    -- ^ ascii  assembly
         | Exe    -- ^ x86    binary
         | Res    -- ^ output of execution
         | Log    -- ^ compile and execution log

instance Show Ext where
  show Src = "egg"
  show Asm = "s"
  show Exe = "run"
  show Res = "result"
  show Log = "log"

ext :: FilePath -> Ext -> FilePath
ext f e = f <.> show e
