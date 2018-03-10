module Language.Egg.TypeInference where
import Language.Egg.Types
import Language.Egg.Utils
improt qualified Data.Map as M
-- Ty (in Types.hs)  defines base types

-- somewhat cribbed from
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

-- | Type variables
newtype TV = TV String deriving (Show, Eq, Ord, IsString)

-- | The type of types
-- higher kinded types are represented as an application of
-- constructor (defined in base type) to some other types.
data Type' baseTypes
  | TVar TV
  | TConst baseTypes
  | (Type baseTypes) :=> (Type baseTypes)
  | TApp (Type' baseTypes) (Type' baseTypes)
  deriving (Show, Eq)

type Type = Type' Ty

-- type Kind = Type' ()
-- star :: Kind
-- star = TConst ()

-- class HasKind baseTypes where
--   kindOf :: baseTypes -> Kind
-- instance HasKind Ty where
--   kindOf TNumber = star
--   kindOf TBoolean = star
--   -- a TTuple is a pair.
--   kindOf TTuple = star :=> star :=> star

-- instance HasKind ty => HasKind (Type' ty) where
--   kindOf (TVar TV) = ???
--   kindOf (TConst ctor) = kindOf ctor
--   kindOf (a :=> b) = kindOf a :=> kindOf b
--   kindOf (TApp (a0 :=> b0) a) | a0 == a = b0
--   kindOf _ = error "kind error"

-- | Existentially quantified types.
data CloseTy baseTypes = Forall [TV] (Type' baseTypes)
type ClosedType = CloseTy Ty

-- typing env
type TypeEnv = M.Map Id ClosedType

-- type substitution map
newtype Subst = Subst (M.Map TV Type)

instance Monoid Subst where
  mempty = Subst M.empty
  mappend (Subst s1) (Subst s2) = Subst $ (apply s1 <$> s2) `M.union` s1

myNub = map head . group . sort

class Substitutable a where
  apply :: Subst -> a -> a
  freevars :: a -> [TV]

instance Substitutable (Type' a) where
  apply s t@(TVar v) = M.findWithDefault t v s
  apply s (TConst c) = TConst c
  apply s (t1 :=> t2) = apply s t1 :=> apply s t2
  apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)

  freevars = myNub . go where
    go (TVar tv) = [tv]
    go (TConst _ ) = []
    go (t1 :=> t2 ) = freevars t1 ++ freevars t2
    go (TApp t1 t2) = freevars t1 ++ freevars t2

instance Substitutable (CloseTy a) where
  apply (Subst s) (Forall tvs t) = Forall tvs (apply s' t)
    where s' = Subst (foldr M.delete s tvs)
  freevars  (Forall tvs t) = freevars t \\ tvs

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  freevars = myNub . foldMap freevars

instance Substitutable TypeEnv  where
  apply s tyEnv = apply s <$> tyEnv
  freevars = freevars . M.elems

type Infer = State Int

fresh :: Infer Type
fresh = do
  s <- get
  put $ s+1
  return $ TVar $ TV $ "a" ++ show s

-- tyvars :: Type' baseTypes -> [TV]
-- tyvars = myNub . go where
--   go (TVar tv) = [tv]
--   go (TConst _) = []
--   go (ty1 :=> ty2) = go ty1 ++ go ty2

generalize :: TypeEnv -> Type' baseTypes -> CloseTy baseTypes
generalize env ty = Forall (freevars ty \\ freevars env) ty

instantiate :: ClosedType -> Infer Type
instantiate (Forall xs t) =
  flip apply t <$> traverse (flip fmap fresh . (,)) xs

unify :: Type -> Type -> Infer ()
unify = undefined


inferE :: AnfExpr a -> Infer (AnfExpr (a, Type))
inferE = snd . runState mempty . go where
  gets b = (M.! b) <$> get

  go (Number n l)  = pure $ Number n (l, TConst TNumber)
  go (Boolean b l) = pure $ Boolean b (l, TConst TBoolean)
  go (Id x l)      = Id x . (l, ) <$> gets x
