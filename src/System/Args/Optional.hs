{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Type-classes to model optional function arguments. Use with care.
--
-- Lots of helpful type-inference is lost in using this technique.
module System.Args.Optional(
  Optional1(..)
, Optional2(..)
) where

import Prelude(id, const)

-- $setup
-- >>> import Prelude(Integer, Bool, String, Num(..), even, odd, (++))
-- >>> import Data.List(reverse, filter)
-- >>> import Data.Char(Char, isUpper, isLower)
-- >>> :set -XFlexibleContexts

-- | One optional argument.
--
-- >>> let timesTwo :: Optional1 Integer Integer x => x; timesTwo = optional1 (*2) 7 in timesTwo :: Integer
-- 14
--
-- >>> let timesTwo :: Optional1 Integer Integer x => x; timesTwo = optional1 (*2) 7 in timesTwo (12 :: Integer) :: Integer
-- 24
--
-- >>> let reverseI :: Optional1 [Integer] [Integer] x => x; reverseI = optional1 reverse ([] :: [Integer]) in reverseI ([1,2,3] :: [Integer]) :: [Integer]
-- [3,2,1]
--
-- >>> let reverseI :: Optional1 [Integer] [Integer] x => x; reverseI = optional1 reverse ([] :: [Integer]) in reverseI :: [Integer]
-- []
--
-- >>> let reverseI :: Optional1 [Integer] [Integer] x => x; reverseI = optional1 reverse [99, 98, 97] in reverseI :: [Integer]
-- [97,98,99]
--
-- >>> let filterI :: Optional1 (Integer -> Bool) [Integer] x => x; filterI = optional1 (\p -> filter p [1..20]) (even :: Integer -> Bool) in filterI :: [Integer]
-- [2,4,6,8,10,12,14,16,18,20]
--
-- >>> let filterI :: Optional1 (Integer -> Bool) [Integer] x => x; filterI = optional1 (\p -> filter p [1..20]) (even :: Integer -> Bool) in filterI (odd :: Integer -> Bool) :: [Integer]
-- [1,3,5,7,9,11,13,15,17,19]
class Optional1 a b x where
  optional1 ::
    (a -> b)
    -> a
    -> x

instance Optional1 a b b where
  optional1 =
    id

instance Optional1 a b (a -> b) where
  optional1 =
    const

-- | Two optional arguments, in the order of @a@ then @b@.
--
-- >>> let append :: Optional2 String String String x => x; append = optional2 (++) "abc" "def" in append :: String
-- "abcdef"
--
-- >>> let append :: Optional2 String String String x => x; append = optional2 (++) "abc" "def" in append "xyz" :: String
-- "xyzdef"
--
-- >>> let append :: Optional2 String String String x => x; append = optional2 (++) "abc" "def" in append "uvw" :: String
-- "uvwdef"
--
-- >>> let append :: Optional2 String String String x => x; append = optional2 (++) "abc" "def" in append "uvw" "xyz" :: String
-- "uvwxyz"
--
-- >>> let filterS :: Optional2 (Char -> Bool) String String x => x; filterS = optional2 filter isUpper "AbCdEfGhI" in filterS :: String
-- "ACEGI"
--
-- >>> let filterS :: Optional2 (Char -> Bool) String String x => x; filterS = optional2 filter isUpper "AbCdEfGhI" in filterS isLower :: String
-- "bdfh"
--
-- >>> let filterS :: Optional2 (Char -> Bool) String String x => x; filterS = optional2 filter isUpper "AbCdEfGhI" in filterS isLower "tUvWxYz" :: String
-- "tvxz"
class Optional2 a b c x where
  optional2 ::
    (a -> b -> c)
    -> a
    -> b
    -> x

instance Optional2 a b c c where
  optional2 =
    id

instance Optional1 b c x => Optional2 a b c (a -> x) where
  optional2 f _ b a =
    optional1 (f a) b
