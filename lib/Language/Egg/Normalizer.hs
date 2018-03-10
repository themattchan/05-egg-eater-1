{-# LANGUAGE FlexibleContexts #-}


--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------

module Language.Egg.Normalizer ( anormal ) where

import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Trans.Writer.Lazy (WriterT(..))
import Control.Monad.Trans.State.Lazy (evalState)


import Language.Egg.Types
import Language.Egg.Utils (assocl)

type Binds a = [(Bind a, (AnfExpr a, a))]

--------------------------------------------------------------------------------
-- | Convert an Expr into A-Normal Form
--------------------------------------------------------------------------------
anormal :: Program a -> AnfProgram a
--------------------------------------------------------------------------------
anormal (Prog ds e) = Prog (anfDecl <$> ds) (anfExpr e)
  where
    anfDecl d       = d { fBody = anfExpr (fBody d) }
    anfExpr e       = evalState (anf e) 0

--------------------------------------------------------------------------------
-- | `anf i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `e'` is equivalent to `e` but is in A-Normal Form.
--------------------------------------------------------------------------------
anf :: MonadState Int m => Expr a -> m (AnfExpr a)
--------------------------------------------------------------------------------
anf (Number n l)      = pure (Number n l)

anf (Boolean b l)     = pure (Boolean b l)

anf (Id     x l)      = pure (Id     x l)

anf (Let x e b l)     = do
  e' <- anf e
  b' <- anf b
  return $ Let x e' b' l

anf (Prim1 o e l)     = do
  (ae, bs) <- runWriterT $ imm e
  return $ stitch bs (Prim1 o ae l)

anf (Prim2 o e1 e2 l) = do
  (ae, bs) <- runWriterT $ do
                e1' <- imm e1
                e2' <- imm e2
                return $ (Prim2 o e1' e2' l)
  return $ stitch bs ae

anf (If c e1 e2 l)    = do
  (c', bs) <- runWriterT $ imm c
  e1' <- anf e1
  e2' <- anf e2
  return $ stitch bs  (If c' e1' e2' l)

anf (App f es l)      = do
  (es', bs) <- runWriterT $ mapM imm es
  return $ stitch bs (App f es' l)

anf (Tuple es l)      = do
  (es', bs) <- runWriterT $ mapM imm es
  return $ stitch bs (Tuple es' l)

anf (GetItem e1 e2 l) = do
  (es', bs) <- runWriterT $ do
                 e1' <- imm e1
                 e2' <- imm e2
                 return $ (GetItem e1' e2' l)
  return $ stitch bs es'

--------------------------------------------------------------------------------
-- | `stitch bs e` takes a "context" `bs` which is a list of temp-vars and their
--   definitions, and an expression `e` that uses the temp-vars in `bs` and glues
--   them together into a `Let` expression.
--------------------------------------------------------------------------------
stitch :: Binds a -> AnfExpr a -> AnfExpr a
--------------------------------------------------------------------------------
stitch bs e = bindsExpr (map (fst . assocl) bs) e (getLabel e)

--------------------------------------------------------------------------------
-- | `imm i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', bs, e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `bs` are the temporary binders needed to render `e` in ANF, and
--   * `e'` is an `imm` value (Id or Number) equivalent to `e`.
--------------------------------------------------------------------------------
imm :: (MonadState Int m, MonadWriter (Binds a) m) => AnfExpr a -> m (ImmExpr a)
--------------------------------------------------------------------------------
imm (Number n l)      = pure $ Number n l

imm (Boolean b  l)    = pure $ Boolean b l

imm (Id x l)          = pure $ Id x l

imm (Prim1 o e1 l)    = do
  v1 <- imm e1
  v  <- fresh l
  tell [(v, (Prim1 o v1 l, l))]
  return $ mkId v l

imm (Prim2 o e1 e2 l) = do
  v1 <- imm e1
  v2 <- imm e2
  v  <- fresh l
  tell [(v, (Prim2 o v1 v2 l, l))]
  return $ mkId v l

imm (Tuple es l)      = do
  vs <- mapM imm es
  v  <- fresh l
  tell [(v, (Tuple vs l, l))]
  return $ mkId v l

imm (GetItem e1 e2 l) = do
  v1 <- imm e1
  v2 <- imm e2
  v  <- fresh l
  tell [(v, (GetItem v1 v2 l, l))]
  return $ mkId v l

imm (App f es l)      = do
  vs <- mapM imm es
  v  <- fresh l
  tell [(v, (App f vs l, l))]
  return $ mkId v l

imm e@(If _ _ _  l)   = immExp e l

imm e@(Let _ _ _ l)   = immExp e l


immExp :: (MonadState Int m, MonadWriter (Binds a) m) => AnfExpr a -> a -> m (ImmExpr a)
immExp e l  = do
  e' <- anf e
  v <- fresh l
  tell [(v, (e', l))]
  return $ mkId v l

mkId :: Bind a -> a -> Expr a
mkId x l = Id (bindId x) l

--------------------------------------------------------------------------------
-- | `fresh i` returns a temp-var named `i` and "increments" the counter
--------------------------------------------------------------------------------
fresh :: MonadState Int m => a -> m (Bind a)
--------------------------------------------------------------------------------
fresh l = do
  i <- get
  put (i+1)
  let x = "anf" ++ show i
  return $ Bind x l
