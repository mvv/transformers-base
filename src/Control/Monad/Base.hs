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
       Applicative η, Monad η) ⇒ MonadBase η μ | μ → η where
  -- | Lift a computation from the base monad
  liftBase ∷ η α → μ α

instance MonadBase IO IO                 where liftBase = id
instance MonadBase Identity Identity     where liftBase = id
instance MonadBase Maybe Maybe           where liftBase = id
instance MonadBase (Either e) (Either e) where liftBase = id
instance MonadBase [] []                 where liftBase = id
instance MonadBase (L.ST s) (L.ST s)     where liftBase = id
instance MonadBase (S.ST s) (S.ST s)     where liftBase = id

instance MonadBase η μ ⇒ MonadBase η (IdentityT μ) where
  liftBase = lift . liftBase
instance MonadBase η μ ⇒ MonadBase η (MaybeT μ) where
  liftBase = lift . liftBase
instance MonadBase η μ ⇒ MonadBase η (ListT μ) where
  liftBase = lift . liftBase
instance MonadBase η μ ⇒ MonadBase η (ReaderT r μ) where
  liftBase = lift . liftBase
instance (Monoid w, MonadBase η μ) ⇒ MonadBase η (L.WriterT w μ) where
  liftBase = lift . liftBase
instance (Monoid w, MonadBase η μ) ⇒ MonadBase η (S.WriterT w μ) where
  liftBase = lift . liftBase
instance MonadBase η μ ⇒ MonadBase η (L.StateT s μ) where
  liftBase = lift . liftBase
instance MonadBase η μ ⇒ MonadBase η (S.StateT s μ) where
  liftBase = lift . liftBase
instance (Monoid w, MonadBase η μ) ⇒ MonadBase η (L.RWST r w s μ) where
  liftBase = lift . liftBase
instance (Monoid w, MonadBase η μ) ⇒ MonadBase η (S.RWST r w s μ) where
  liftBase = lift . liftBase
instance (Error e, MonadBase η μ) ⇒ MonadBase η (ErrorT e μ) where
  liftBase = lift . liftBase
instance MonadBase η μ ⇒ MonadBase η (ContT r μ) where
  liftBase = lift . liftBase

