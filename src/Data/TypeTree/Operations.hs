-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.Operations
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | A variety of type-level functions.
--
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Data.TypeTree.Operations (
    -- ** Numeric operations
    DifferenceLTEQ,
    -- * Type-level booleans
    Yes, No, If, Or, And, Xor, Not, TypeEquals, TypeUnequals,
    -- * Repeated applications of type constructors
    InitTypes, LastType, TypeFold,
    -- * Other miscellaneous type-level functions
    Flip(..), Compose(..), Compare
) where

import Data.Peano

-- | 'Yes' if @abs(b - c) <= a@, 'No' otherwise.
type family DifferenceLTEQ a b c where
    DifferenceLTEQ a        Zero     Zero     = Yes
    DifferenceLTEQ Zero     (Succ b) Zero     = No
    DifferenceLTEQ Zero     Zero     (Succ c) = No
    DifferenceLTEQ (Succ a) (Succ b) Zero     = DifferenceLTEQ a b Zero
    DifferenceLTEQ (Succ a) Zero     (Succ c) = DifferenceLTEQ a Zero c
    DifferenceLTEQ a        (Succ b) (Succ c) = DifferenceLTEQ a b c

 -----------------------------------------------------------------------------
-- Equality and inequality
-----------------------------------------------------------------------------

-- | For dealing with dependencies that don't support 'PolyKinds', we roll our
--   own type-level 'Bool' as well. Too bad, since otherwise we could have used
--   'Data.Type.Bool.Or' and the like.
data Yes
data No

type family If a b c where
    If Yes b c = b
    If No  b c = c

type family Or a b where
    Or No  No  = No
    Or Yes No  = Yes
    Or No  Yes = Yes
    Or Yes Yes = Yes

type family And a b where
    And Yes Yes = Yes
    And No  Yes = No
    And Yes No  = No
    And No  No  = No

type family Xor a b where
    Xor No  No  = No
    Xor Yes No  = Yes
    Xor No  Yes = Yes
    Xor Yes Yes = No

type family Not a where
    Not Yes = No
    Not No  = Yes

-- | Polykinded type equality. See also "Data.Type.Equality"
type family TypeEquals a b where
    TypeEquals a a = Yes
    TypeEquals a b = No

-- | It would be quite nice if this could be eliminated, but unfortunately it
--   is sometimes useful to eliminate nested type family applications, whereas
--   'Not (a == b)' would require '-XUndecideableInstances'.
type family TypeUnequals a b where
    TypeUnequals a a = No
    TypeUnequals a b = Yes

-----------------------------------------------------------------------------
-- Other
-----------------------------------------------------------------------------

-- | Get the last argument to a repeatedly-applied type constructor 'f'. This
--   is useful, for example, to get the final output type of a function, or
--   the end of a list
type family LastType f a where
    LastType f (f a (f b c)) = LastType f (f b c)
    LastType f (f b c) = c

-- | Get all but the last argument to a repeatedly-applied type constructor
--   'f', and replace all applications of 'f' with 'k'. This is useful, for
--   example, to get the types of all arguments of a function, or the 'init'
--   of a type-level list.
type family InitTypes f k a where
    InitTypes f k (f a (f b c)) = k a (InitTypes f k (f b c))
    InitTypes f k (f b c) = b

-- | This replaces /all/ applications of a repeatedly-applied type constructor
--   'f' with 'k', unlike 'InitTypes' which leaves off the last application.
type family TypeFold f k a where
    TypeFold f k (f a b) = k a (TypeFold f k b)
    TypeFold f k a       = a

-- | Flips the order of arguments in a type constructor. This has to be
--   a newtype rather than a type synonym or type family, since the latter
--   two can't be partially-applied; typically the whole point of using 'Flip'
--   is so that you can make, e.g., a 'Functor' instance for 'f a'.
newtype Flip f a b = Flip { unflip :: f b a }

-- | Type-level equivalent of function compositiion. For example, given 'c'
--   of kind '* -> Constraint' and 'Dict' of kind 'Constraint -> *', we
--   can write 'Compose Dict c' and 'MapArgs' it on to a list or tree.
newtype Compose f g a = Compose { _decompose :: f (g a) }

-- | Type-level comparisons. Unlike equality, there's no real choice but to
--   exhaustively write out examples. Replace this with Nicolas Frisby's
--   "Type.Ord" once "Type.Spine" on Hackage is updated to deal with Template
--   Haskell 2.9.
type family Compare (a :: k1) (b :: k2) :: Ordering
