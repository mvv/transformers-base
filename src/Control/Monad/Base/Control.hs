{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Base.Control (
    MonadBaseControl(..),
    controlBase,
    liftBaseOp,
    liftBaseOp_
  ) where

import Data.Monoid
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Control

class MonadBase μ η ⇒ MonadBaseControl μ η | μ → η where
  -- | Helper for lifting control operations from the base monad.
  liftBaseControl ∷ (RunInBase μ η → η α) → μ α

instance MonadBaseControl IO IO where
  liftBaseControl = idLiftControl

instance MonadBaseControl μ η ⇒ MonadBaseControl (IdentityT μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl μ η ⇒ MonadBaseControl (MaybeT μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Error e, MonadBaseControl μ η)
       ⇒ MonadBaseControl (ErrorT e μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl μ η ⇒ MonadBaseControl (ListT μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl μ η ⇒ MonadBaseControl (ReaderT r μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Monoid w, MonadBaseControl μ η)
       ⇒ MonadBaseControl (L.WriterT w μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Monoid w, MonadBaseControl μ η)
       ⇒ MonadBaseControl (S.WriterT w μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl μ η ⇒ MonadBaseControl (L.StateT s μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl μ η ⇒ MonadBaseControl (S.StateT s μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Monoid w, MonadBaseControl μ η)
       ⇒ MonadBaseControl (L.RWST r w s μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Monoid w, MonadBaseControl μ η)
       ⇒ MonadBaseControl (S.RWST r w s μ) η where
  liftBaseControl = liftLiftControlBase liftBaseControl

controlBase ∷ MonadBaseControl μ η ⇒ (RunInBase μ η → η (μ α)) → μ α
controlBase = join . liftBaseControl

liftBaseOp ∷ MonadBaseControl μ η
           ⇒ ((α → η (μ β)) → η (μ γ))
           →  (α →    μ β ) →    μ γ
liftBaseOp base m = controlBase $ \runInBase → base $ runInBase . m

liftBaseOp_ ∷ MonadBaseControl μ η
           ⇒ (η (μ β) → η (μ γ))
           →     μ β  →    μ γ
liftBaseOp_ base m = controlBase $ \runInBase → base $ runInBase m

