{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ConstraintKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Data.Lens.Constraints where

import Control.Category
import Control.Exception
import Control.Exception.Lens (exception)
import Control.Lens
import Data.Constraint
import Data.Proxy
import Prelude hiding ((.),id)

class (p ~ (->), Functor f) => LensConstraint p f
class (LensConstraint p f, Applicative f) => TraversalConstraint p f
class (p ~ (->), Settable f) => SetterConstraint p f
class (TraversalConstraint p f, Contravariant f) => FoldConstraint p f
class (LensConstraint p f, Contravariant f) => GetterConstraint p f
class (Choice p, Bifunctor p, Applicative f) => ReviewConstraint p f
class (Choice p, Applicative f) => PrismConstraint p f
class (Profunctor p, Functor f) => IsoConstraint p f
class EqualityConstraint (p :: * -> * -> *) (f :: * -> *)

instance (p ~ (->), Functor f) => LensConstraint p f
instance (LensConstraint p f, Applicative f) => TraversalConstraint p f
instance (TraversalConstraint p f, Contravariant f) => FoldConstraint p f
instance (p ~ (->), Settable f) => SetterConstraint p f
instance (LensConstraint p f, Contravariant f) => GetterConstraint p f
instance (Choice p, Bifunctor p, Applicative f) => ReviewConstraint p f
instance (Choice p, Applicative f) => PrismConstraint p f
instance (Profunctor p, Functor f) => IsoConstraint p f
instance EqualityConstraint p f 

class (s :: (* -> * -> *) -> (* -> *) -> Constraint) :< (t :: (* -> * -> *) -> (* -> *) -> Constraint) where
  impl :: t p f :- s p f

instance SetterConstraint :< SetterConstraint where impl = id
instance GetterConstraint :< GetterConstraint where impl = id
instance GetterConstraint :< FoldConstraint where impl = Sub Dict
instance FoldConstraint :< FoldConstraint where impl = id
instance TraversalConstraint :< FoldConstraint where impl = Sub Dict
instance TraversalConstraint :< SetterConstraint where impl = Sub Dict
instance TraversalConstraint :< TraversalConstraint where impl = Sub Dict
instance PrismConstraint :< FoldConstraint where impl = Sub Dict
instance PrismConstraint :< SetterConstraint where impl = Sub Dict
instance PrismConstraint :< ReviewConstraint where impl = Sub Dict
instance PrismConstraint :< TraversalConstraint where impl = Sub Dict 
instance PrismConstraint :< PrismConstraint where impl = id
instance ReviewConstraint :< ReviewConstraint where impl = id
instance LensConstraint :< GetterConstraint where impl = Sub Dict
instance LensConstraint :< FoldConstraint where impl = Sub Dict
instance LensConstraint :< SetterConstraint where impl = Sub Dict
instance LensConstraint :< TraversalConstraint where impl = Sub Dict
instance LensConstraint :< LensConstraint where impl = id
instance IsoConstraint :< GetterConstraint where impl = Sub Dict
instance IsoConstraint :< FoldConstraint where impl = Sub Dict
instance IsoConstraint :< SetterConstraint where impl = Sub Dict
instance IsoConstraint :< ReviewConstraint where impl = Sub Dict
instance IsoConstraint :< TraversalConstraint where impl = Sub Dict
instance IsoConstraint :< PrismConstraint where impl = Sub Dict
instance IsoConstraint :< IsoConstraint where impl = id
instance EqualityConstraint :< GetterConstraint where impl = Sub Dict
instance EqualityConstraint :< FoldConstraint where impl = Sub Dict
instance EqualityConstraint :< SetterConstraint where impl = Sub Dict
instance EqualityConstraint :< IsoConstraint where impl = Sub Dict
instance EqualityConstraint :< ReviewConstraint where impl = Sub Dict
instance EqualityConstraint :< TraversalConstraint where impl = Sub Dict
instance EqualityConstraint :< PrismConstraint where impl = Sub Dict
instance EqualityConstraint :< LensConstraint where impl = Sub Dict
instance EqualityConstraint :< EqualityConstraint where impl = id

class (s :< u, t :< u) => Join s t u | s t -> u where
instance Join FoldConstraint FoldConstraint FoldConstraint
instance Join FoldConstraint GetterConstraint FoldConstraint
instance Join FoldConstraint TraversalConstraint FoldConstraint
instance Join FoldConstraint LensConstraint FoldConstraint
instance Join FoldConstraint PrismConstraint FoldConstraint
instance Join GetterConstraint FoldConstraint FoldConstraint
instance Join GetterConstraint GetterConstraint GetterConstraint
instance Join GetterConstraint TraversalConstraint FoldConstraint
instance Join GetterConstraint LensConstraint GetterConstraint
instance Join GetterConstraint PrismConstraint FoldConstraint
instance Join SetterConstraint SetterConstraint SetterConstraint
instance Join SetterConstraint TraversalConstraint SetterConstraint
instance Join SetterConstraint LensConstraint SetterConstraint
instance Join SetterConstraint PrismConstraint SetterConstraint
instance Join TraversalConstraint TraversalConstraint TraversalConstraint
instance Join TraversalConstraint FoldConstraint FoldConstraint
instance Join TraversalConstraint SetterConstraint SetterConstraint
instance Join TraversalConstraint LensConstraint TraversalConstraint
instance Join TraversalConstraint PrismConstraint TraversalConstraint
instance Join LensConstraint LensConstraint LensConstraint
instance Join LensConstraint GetterConstraint GetterConstraint
instance Join LensConstraint FoldConstraint FoldConstraint
instance Join LensConstraint SetterConstraint SetterConstraint
instance Join LensConstraint TraversalConstraint TraversalConstraint
instance Join LensConstraint PrismConstraint TraversalConstraint
instance Join PrismConstraint PrismConstraint PrismConstraint
instance Join PrismConstraint GetterConstraint FoldConstraint
instance Join PrismConstraint FoldConstraint FoldConstraint
instance Join PrismConstraint SetterConstraint SetterConstraint
instance Join PrismConstraint TraversalConstraint TraversalConstraint
instance Join PrismConstraint LensConstraint TraversalConstraint
instance Join PrismConstraint ReviewConstraint ReviewConstraint
instance Join ReviewConstraint ReviewConstraint ReviewConstraint
instance Join ReviewConstraint PrismConstraint ReviewConstraint

class AsArithException t where
  type AsArithExceptionConstraint t :: (* -> * -> *) -> (* -> *) -> Constraint
  type AsArithExceptionConstraint t = PrismConstraint

  _AsArithException' :: AsArithExceptionConstraint t p f => Optic' p f t ArithException


  _AsArithExceptionIsPrism :: proxy t -> Dict (AsArithExceptionConstraint t :< PrismConstraint)
  default _AsArithExceptionIsPrism :: (AsArithExceptionConstraint t ~ PrismConstraint) => proxy t -> Dict (AsArithExceptionConstraint t :< PrismConstraint)
  _AsArithExceptionIsPrism _ = Dict
  

_AsArithException :: forall t p f. (AsArithException t, Choice p, Applicative f) => Optic' p f t ArithException
_AsArithException = case _AsArithExceptionIsPrism (Proxy :: Proxy t) of
  Dict -> case impl :: PrismConstraint p f :- AsArithExceptionConstraint t p f of
    Sub Dict -> _AsArithException' 
   
instance AsArithException ArithException where
  type AsArithExceptionConstraint ArithException = EqualityConstraint
  _AsArithExceptionIsPrism _ = Dict
  _AsArithException' = id

instance AsArithException SomeException where
  _AsArithException' = exception
