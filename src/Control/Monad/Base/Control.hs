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

class MonadBase η μ ⇒ MonadBaseControl η μ | μ → η where
  -- | Helper for lifting control operations from the base monad.
  liftBaseControl ∷ (RunInBase μ η → η α) → μ α

instance MonadBaseControl IO IO where
  liftBaseControl = idLiftControl

instance MonadBaseControl η μ ⇒ MonadBaseControl η (IdentityT μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl η μ ⇒ MonadBaseControl η (MaybeT μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Error e, MonadBaseControl η μ)
       ⇒ MonadBaseControl η (ErrorT e μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl η μ ⇒ MonadBaseControl η (ListT μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl η μ ⇒ MonadBaseControl η (ReaderT r μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Monoid w, MonadBaseControl η μ)
       ⇒ MonadBaseControl η (L.WriterT w μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Monoid w, MonadBaseControl η μ)
       ⇒ MonadBaseControl η (S.WriterT w μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl η μ ⇒ MonadBaseControl η (L.StateT s μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance MonadBaseControl η μ ⇒ MonadBaseControl η (S.StateT s μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Monoid w, MonadBaseControl η μ)
       ⇒ MonadBaseControl η (L.RWST r w s μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

instance (Monoid w, MonadBaseControl η μ)
       ⇒ MonadBaseControl η (S.RWST r w s μ) where
  liftBaseControl = liftLiftControlBase liftBaseControl

controlBase ∷ MonadBaseControl η μ ⇒ (RunInBase μ η → η (μ α)) → μ α
controlBase = join . liftBaseControl

liftBaseOp ∷ MonadBaseControl η μ
           ⇒ ((α → η (μ β)) → η (μ γ))
           →  (α →    μ β ) →    μ γ
liftBaseOp base m = controlBase $ \runInBase → base $ runInBase . m

liftBaseOp_ ∷ MonadBaseControl η μ
           ⇒ (η (μ β) → η (μ γ))
           →     μ β  →    μ γ
liftBaseOp_ base m = controlBase $ \runInBase → base $ runInBase m

