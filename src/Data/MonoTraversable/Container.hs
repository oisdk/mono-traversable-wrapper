{-# LANGUAGE GADTs #-}

-- | This module provides the 'Container' type, which wraps monomorphic types,
-- and makes them conform to the "Prelude"'s non-monomorphic typeclasses.
module Data.MonoTraversable.Container where

import           Data.Foldable
import           Data.MonoTraversable

import           Text.Read

-- | This type can wrap a monomorphic  type to allow it to conform to
-- 'Foldable'. For instance, the wrapper for 'Data.Text.Text' would be
-- defined as:
--
-- @type FoldableText = 'Container' 'Text' 'Char'@
--
-- Now, this type conforms to 'Foldable'.
data Container xs a b where
        -- | Can only construct when last type variable is equal to second.
        Container :: (a ~ b) => xs -> Container xs a b

-- | Selector for 'Container'
getContainer :: Container xs a b -> xs
getContainer (Container xs) = xs
{-# INLINE getContainer #-}

instance Eq xs => Eq (Container xs a b) where
  Container x == Container y = x == y
  {-# INLINE (==) #-}

instance Ord xs => Ord (Container xs a b) where
  compare (Container x) (Container y) = compare x y
  {-# INLINE compare #-}
  Container x < Container y = x < y
  {-# INLINE (<) #-}
  Container x >= Container y = x >= y
  {-# INLINE (>=) #-}
  Container x > Container y = x > y
  {-# INLINE (>) #-}
  Container x <= Container y = x <= y
  {-# INLINE (<=) #-}
  max (Container x) (Container y) = Container (max x y)
  {-# INLINE max #-}
  min (Container x) (Container y) = Container (min x y)
  {-# INLINE min #-}

instance Show xs => Show (Container xs a b) where
  showsPrec d (Container a) =
    showParen (d >= 11)
      $ showString "Container "
      . showsPrec 11 a

instance (Read xs, a ~ b) => Read (Container xs a b) where
  readPrec = parens $ prec 10 $ do
    Ident "Container" <- lexP
    Container <$> step (readS_to_Prec readsPrec)

instance (MonoFoldable xs, element ~ Element xs) =>
         Foldable (Container xs element) where
    foldr f b (Container xs) = ofoldr f b xs
    {-# INLINE foldr #-}
    foldMap f (Container xs) = ofoldMap f xs
    {-# INLINE foldMap #-}
    foldl' f b (Container xs) = ofoldl' f b xs
    {-# INLINE foldl' #-}
    toList (Container xs) = otoList xs
    {-# INLINE toList #-}
    null (Container xs) = onull xs
    {-# INLINE null #-}
    length (Container xs) = olength xs
    {-# INLINE length #-}
    foldr1 f (Container xs) = ofoldr1Ex f xs
    {-# INLINE foldr1 #-}
    elem x (Container xs) = oelem x xs
    {-# INLINE elem #-}
    maximum (Container xs) = maximumEx xs
    {-# INLINE maximum #-}
    minimum (Container xs) = minimumEx xs
    {-# INLINE minimum #-}
    sum (Container xs) = osum xs
    {-# INLINE sum #-}
    product (Container xs) = oproduct xs
    {-# INLINE product #-}
