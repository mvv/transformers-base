{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Base (
    MonadBase(..)
  ) where

import Data.Monoid
import Data.Functor.Identity
import Control.Applicative
import qualified Control.Monad.ST.Lazy as L
import qualified Control.Monad.ST.Strict as S
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Error
import Control.Monad.Trans.Cont

class (Applicative μ, Monad μ,
       Applicative η, Monad η) ⇒ MonadBase μ η | μ → η where
  -- | Lift a computation from the base monad
  liftBase ∷ η α → μ α

instance MonadBase IO IO                 where liftBase = id
instance MonadBase Identity Identity     where liftBase = id
instance MonadBase Maybe Maybe           where liftBase = id
instance MonadBase (Either e) (Either e) where liftBase = id
instance MonadBase [] []                 where liftBase = id
instance MonadBase (L.ST s) (L.ST s)     where liftBase = id
instance MonadBase (S.ST s) (S.ST s)     where liftBase = id

instance MonadBase μ η ⇒ MonadBase (IdentityT μ) η where
  liftBase = lift . liftBase
instance MonadBase μ η ⇒ MonadBase (MaybeT μ) η where
  liftBase = lift . liftBase
instance MonadBase μ η ⇒ MonadBase (ListT μ) η where
  liftBase = lift . liftBase
instance MonadBase μ η ⇒ MonadBase (ReaderT r μ) η where
  liftBase = lift . liftBase
instance (Monoid w, MonadBase μ η) ⇒ MonadBase (L.WriterT w μ) η where
  liftBase = lift . liftBase
instance (Monoid w, MonadBase μ η) ⇒ MonadBase (S.WriterT w μ) η where
  liftBase = lift . liftBase
instance MonadBase μ η ⇒ MonadBase (L.StateT s μ) η where
  liftBase = lift . liftBase
instance MonadBase μ η ⇒ MonadBase (S.StateT s μ) η where
  liftBase = lift . liftBase
instance (Monoid w, MonadBase μ η) ⇒ MonadBase (L.RWST r w s μ) η where
  liftBase = lift . liftBase
instance (Monoid w, MonadBase μ η) ⇒ MonadBase (S.RWST r w s μ) η where
  liftBase = lift . liftBase
instance (Error e, MonadBase μ η) ⇒ MonadBase (ErrorT e μ) η where
  liftBase = lift . liftBase
instance MonadBase μ η ⇒ MonadBase (ContT r μ) η where
  liftBase = lift . liftBase

