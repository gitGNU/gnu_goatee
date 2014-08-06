-- This file is part of Goatee.
--
-- Copyright 2014 Bryan Gardiner
--
-- Goatee is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Goatee is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with Goatee.  If not, see <http://www.gnu.org/licenses/>.

-- | Base-10 arbitrary-precision floating-point numbers.
module Game.Goatee.Common.Bigfloat (
  Bigfloat, encode,
  significand, exponent,
  fromDouble, toDouble,
  ) where

import Data.Char (isDigit, isSpace)
import Data.Function (on)
import Prelude hiding (exponent, significand)

-- | A base-10, infinite-precision, floating-point number.  Implemented as an
-- infinite-precision significand together with an exponent, such that the
-- numeric value is equal to @'significand' f * (10 ^ 'exponent' f)@.  The
-- exponent is a limited-precision 'Int', because some operations may break if
-- the exponent is larger (specifically 'show' and 'toDouble').  This shouldn't
-- be an issue for Goatee.
--
-- These values form an integral domain.
--
-- The 'Show' instance always outputs in decimal notation, never scientific
-- notation.  Examples:
--
-- > 300   (never trailing .0 if there's no fractional part)
-- > 0.1   (never redundant trailing or leading zeros)
--
-- Similarly, the 'Read' instance accepts numbers matching the regex
-- @-?\\d+(\\.\\d+)?(e-?\\d+)?@.  Scientific exponent notation is supported for
-- reading, for ease of converting 'Double's to 'Bigfloat's.
data Bigfloat = Bigfloat {
  significand :: !Integer
  , exponent :: !Int
  }

zero, one, negOne :: Bigfloat
zero = Bigfloat 0 0
one = Bigfloat 1 0
negOne = Bigfloat (-1) 0

instance Eq Bigfloat where
  x == y = let (Bigfloat xv xe, Bigfloat yv ye) = normalize2 x y
           in xe == ye && xv == yv

instance Ord Bigfloat where
  compare = (uncurry (compare `on` significand) .) . normalize2

instance Num Bigfloat where
  (+) = lift2 (+)
  (-) = lift2 (-)
  Bigfloat xv xe * Bigfloat yv ye = reduce $ Bigfloat (xv * yv) (xe + ye)
  negate (Bigfloat v e) = Bigfloat (-v) e
  abs x@(Bigfloat v e) = if v >= 0 then x else Bigfloat (-v) e
  signum (Bigfloat v _)
    | v == 0 = zero
    | v > 0 = one
    | otherwise = negOne
  fromInteger v = reduce $ Bigfloat v 0

instance Show Bigfloat where
  show (Bigfloat v e) =
    let (addSign, vs) = if v >= 0
                        then (id, show v)
                        else (('-':), show (-v))
        vl = length vs
    in addSign $ case e of
      0 -> vs
      e | e > 0 -> vs ++ replicate e '0'
        | e <= -vl -> '0' : '.' : replicate ((-e) - vl) '0' ++ vs
      _ -> let (hd, tl) = splitAt (vl + e) vs
           in hd ++ '.' : tl

instance Read Bigfloat where
  readsPrec _ s =
    let (s', neg) = case s of
          '-':s' -> (s', True)
          _ -> (s, False)
        (whole, s'') = span isDigit s'
    in if null whole
       then []
       else case s'' of
         '.':s''' -> let (fractional, s'''') = span isDigit s'''
                     in if null fractional
                        then []
                        else succeedIfTerminatedProperly neg whole fractional s''''
         s''' -> succeedIfTerminatedProperly neg whole [] s'''
    where succeedIfTerminatedProperly neg whole fractional rest =
            let makeResult exp =
                  encode (fromInteger $
                          read $
                          (if neg then ('-':) else id) $
                          whole ++ fractional)
                         (-length fractional + exp)
            in if isValidEndOfNumber rest
               then [(makeResult 0, rest)]
               else case rest of
                 'e':exps -> let (addExpNeg, exps') = case exps of
                                   '-':exps' -> (('-':), exps')
                                   _ -> (id, exps)
                                 (hd, tl) = span isDigit exps'
                             in if null hd
                                then []
                                else let exp = read (addExpNeg exps') :: Int
                                     in [(makeResult exp, tl) | isValidEndOfNumber tl]
                 _ -> []
          isValidEndOfNumber rest = case rest of
            [] -> True
            c:_ | isSpace c -> True
            _ -> False

-- | @encode significand exponent@ creates a 'Bigfloat' value whose numeric
-- value is @significand * (10 ^ exponent)@.
encode :: Integer -> Int -> Bigfloat
encode = (reduce .) . Bigfloat

-- | Converts a 'Double' to a 'Bigfloat' (with as much precision as the 'Double'
-- 'Show' instance provides).
fromDouble :: Double -> Bigfloat
fromDouble = read . show

-- | Converts a 'Bigfloat' to a 'Double', lossily.
toDouble :: Bigfloat -> Double
toDouble = read . show

-- | @shift amount float@ adds @shift@ zeros onto the right side of @float@'s
-- numerator while keeping the numeric value the same.  @amount@ must be
-- non-negative.
shift :: Int -> Bigfloat -> Bigfloat
shift amount float@(Bigfloat v e) =
  if amount < 0
  then error $ "bigfloatShift: Can't shift by a negative amount.  amount = " ++
       show amount ++ ", float = " ++ show float
  else Bigfloat (v * 10 ^ amount) (e - amount)

-- | Reduces a 'Bigfloat' to canonical form, keeping the numeric value the same
-- but removing trailing zeros from the numerator.
reduce :: Bigfloat -> Bigfloat
reduce x@(Bigfloat v e) =
  if v == 0
  then zero
  else let zeros = length $ takeWhile (== '0') $ reverse $ show v
       in if zeros == 0
          then x
          else Bigfloat (v `div` (10 ^ zeros)) (e + zeros)

-- | Converts two 'Bigfloat's so that they have the same number of decimal
-- places, so that 'Integer' arithmetic may be performed directly on their
-- 'significand's.
normalize2 :: Bigfloat -> Bigfloat -> (Bigfloat, Bigfloat)
normalize2 x y =
  let xe = exponent x
      ye = exponent y
  in if xe == ye
     then (x, y)
     else if xe < ye
          then (x, shift (ye - xe) y)
          else (shift (xe - ye) x, y)

-- | Lifts a function on two 'Integer's to a function on 'Bigfloat's.
--
-- This is not exported from this module because it's not a general lift
-- function: the given function only operates on the significands, so operations
-- that require the exponent (such as multiplication) can't use this function.
lift2 :: (Integer -> Integer -> Integer) -> Bigfloat -> Bigfloat -> Bigfloat
lift2 f x y =
  let (Bigfloat xv xe, Bigfloat yv _) = normalize2 x y
  in reduce $ Bigfloat (f xv yv) xe
