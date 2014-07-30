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

module Game.Goatee.Common.BigfloatTest (tests) where

import Game.Goatee.Common.Bigfloat
import Game.Goatee.Test.Common
import Prelude hiding (exponent, significand)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?), Assertion)

assertBigfloat :: Integer -> Int -> Bigfloat -> Assertion
assertBigfloat v e x =
  (v, e) @=? (significand x, exponent x)

testAddition :: String -> String -> Integer -> Int -> Test
testAddition x y v e =
  testCase (x ++ " + " ++ y) $ assertBigfloat v e $ read x + read y

testSubtraction :: String -> String -> Integer -> Int -> Test
testSubtraction x y v e =
  testCase (x ++ " - " ++ y) $ assertBigfloat v e $ read x - read y

testMultiplication :: String -> String -> Integer -> Int -> Test
testMultiplication x y v e =
  testCase (x ++ " * " ++ y) $ assertBigfloat v e $ read x * read y

tests = testGroup "Game.Goatee.Common.Bigfloat"
  [ encodeTests
  , readInstanceTests
  , showInstanceTests
  , fromDoubleTests
  , toDoubleTests
  , numInstanceTests
  , eqInstanceTests
  , ordInstanceTests
  ]

encodeTests = testGroup "encode" [
  testCase "zero" $ assertBigfloat 0 0 $ encode 0 0,

  testGroup "whole numbers" [
    testCase "1" $ assertBigfloat 1 0 $ encode 1 0,
    testCase "5" $ assertBigfloat 5 0 $ encode 5 0,
    testCase "12" $ assertBigfloat 12 0 $ encode 12 0,
    testCase "1031" $ assertBigfloat 1031 0 $ encode 1031 0,

    testCase "-1" $ assertBigfloat (-1) 0 $ encode (-1) 0,
    testCase "-5" $ assertBigfloat (-5) 0 $ encode (-5) 0,
    testCase "-12" $ assertBigfloat (-12) 0 $ encode (-12) 0,
    testCase "-1031" $ assertBigfloat (-1031) 0 $ encode (-1031) 0
    ],

  testGroup "fractional numbers" [
    testCase "0.1" $ assertBigfloat 1 (-1) $ encode 1 (-1),
    testCase "0.5" $ assertBigfloat 5 (-1) $ encode 5 (-1),
    testCase "1.5" $ assertBigfloat 15 (-1) $ encode 15 (-1),
    testCase "123.456" $ assertBigfloat 123456 (-3) $ encode 123456 (-3),
    testCase "0.00125" $ assertBigfloat 125 (-5) $ encode 125 (-5),

    testCase "-0.1" $ assertBigfloat (-1) (-1) $ encode (-1) (-1),
    testCase "-0.5" $ assertBigfloat (-5) (-1) $ encode (-5) (-1),
    testCase "-1.5" $ assertBigfloat (-15) (-1) $ encode (-15) (-1),
    testCase "-123.456" $ assertBigfloat (-123456) (-3) $ encode (-123456) (-3),
    testCase "-0.00125" $ assertBigfloat (-125) (-5) $ encode (-125) (-5)
    ],

  testGroup "extreme numbers" [
    testCase "12345678901234567890123456789012345678901234567890" $
      assertBigfloat 1234567890123456789012345678901234567890123456789 1 $
      encode 12345678901234567890123456789012345678901234567890 0,

    testCase "0.00000000000000000000000000000000000000000000000001" $
      assertBigfloat 1 (-50) $ encode 1 (-50),

    testCase "123456789012345678901234567890.123456789012345678901234567890" $
      assertBigfloat 12345678901234567890123456789012345678901234567890123456789 (-29) $
      encode 123456789012345678901234567890123456789012345678901234567890 (-30)
    ],

  testGroup "reduction" [
    testCase "50.0" $ assertBigfloat 5 1 $ encode 500 (-1),
    testCase "123.456000" $ assertBigfloat 123456 (-3) $ encode 123456000 (-6),

    testCase "-50.0" $ assertBigfloat (-5) 1 $ encode (-500) (-1),
    testCase "-123.456000" $ assertBigfloat (-123456) (-3) $ encode (-123456000) (-6)
    ]
  ]

readInstanceTests = testGroup "Read instance" [
  testCase "0" $ assertBigfloat 0 0 $ read "0",
  testCase "3" $ assertBigfloat 3 0 $ read "3",
  testCase "10" $ assertBigfloat 1 1 $ read "10",
  testCase "51200" $ assertBigfloat 512 2 $ read "51200",
  testCase "5120" $ assertBigfloat 512 1 $ read "5120",
  testCase "512" $ assertBigfloat 512 0 $ read "512",
  testCase "512.0" $ assertBigfloat 512 0 $ read "512.0",
  testCase "51.2" $ assertBigfloat 512 (-1) $ read "51.2",
  testCase "05.12" $ assertBigfloat 512 (-2) $ read "05.12",
  testCase "0.512" $ assertBigfloat 512 (-3) $ read "0.512",
  testCase "0.051200" $ assertBigfloat 512 (-4) $ read "0.051200",

  testCase "-0" $ assertBigfloat 0 0 $ read "-0",
  testCase "-3" $ assertBigfloat (-3) 0 $ read "-3",
  testCase "-10" $ assertBigfloat (-1) 1 $ read "-10",
  testCase "-51200" $ assertBigfloat (-512) 2 $ read "-51200",
  testCase "-5120" $ assertBigfloat (-512) 1 $ read "-5120",
  testCase "-512" $ assertBigfloat (-512) 0 $ read "-512",
  testCase "-512.0" $ assertBigfloat (-512) 0 $ read "-512.0",
  testCase "-51.2" $ assertBigfloat (-512) (-1) $ read "-51.2",
  testCase "-05.12" $ assertBigfloat (-512) (-2) $ read "-05.12",
  testCase "-0.512" $ assertBigfloat (-512) (-3) $ read "-0.512",
  testCase "-0.051200" $ assertBigfloat (-512) (-4) $ read "-0.051200",

  testCase "0e0" $ assertBigfloat 0 0 $ read "0e0",
  testCase "0e-0" $ assertBigfloat 0 0 $ read "0e-0",
  testCase "0e1" $ assertBigfloat 0 0 $ read "0e1",
  testCase "0e-1" $ assertBigfloat 0 0 $ read "0e-1",
  testCase "0.0e0" $ assertBigfloat 0 0 $ read "0.0e0",
  testCase "1e0" $ assertBigfloat 1 0 $ read "1e0",
  testCase "1e1" $ assertBigfloat 1 1 $ read "1e1",
  testCase "1e2" $ assertBigfloat 1 2 $ read "1e2",
  testCase "1e-1" $ assertBigfloat 1 (-1) $ read "1e-1",
  testCase "1e-2" $ assertBigfloat 1 (-2) $ read "1e-2",
  testCase "100e2" $ assertBigfloat 1 4 $ read "100e2",
  testCase "100e-1" $ assertBigfloat 1 1 $ read "100e-1",
  testCase "100e-3" $ assertBigfloat 1 (-1) $ read "100e-3",
  testCase "0.0002e-5" $ assertBigfloat 2 (-9) $ read "0.0002e-5",
  testCase "0.0002e2" $ assertBigfloat 2 (-2) $ read "0.0002e2",
  testCase "0.0002e5" $ assertBigfloat 2 1 $ read "0.0002e5"
  ]

showInstanceTests = testGroup "Show instance" [
  testCase "0" $ "0" @=? show (encode 0 0),

  testCase "3" $ "3" @=? show (encode 3 0),
  testCase "10" $ "10" @=? show (encode 1 1),
  testCase "51200" $ "51200" @=? show (encode 512 2),
  testCase "5120" $ "5120" @=? show (encode 512 1),
  testCase "512" $ "512" @=? show (encode 512 0),
  testCase "51.2" $ "51.2" @=? show (encode 512 (-1)),
  testCase "5.12" $ "5.12" @=? show (encode 512 (-2)),
  testCase "0.512" $ "0.512" @=? show (encode 512 (-3)),
  testCase "0.0512" $ "0.0512" @=? show (encode 512 (-4)),

  testCase "-3" $ "-3" @=? show (encode (-3) 0),
  testCase "-10" $ "-10" @=? show (encode (-10) 0),
  testCase "-51200" $ "-51200" @=? show (encode (-512) 2),
  testCase "-5120" $ "-5120" @=? show (encode (-512) 1),
  testCase "-512" $ "-512" @=? show (encode (-512) 0),
  testCase "-51.2" $ "-51.2" @=? show (encode (-512) (-1)),
  testCase "-5.12" $ "-5.12" @=? show (encode (-512) (-2)),
  testCase "-0.512" $ "-0.512" @=? show (encode (-512) (-3)),
  testCase "-0.0512" $ "-0.0512" @=? show (encode (-512) (-4))
  ]

fromDoubleTests = testGroup "fromDouble" [
  testCase "0" $ read "0" @=? fromDouble 0,
  testCase "120" $ read "120" @=? fromDouble 120,
  testCase "12" $ read "12" @=? fromDouble 12,
  testCase "1.2" $ read "1.2" @=? fromDouble 1.2,
  testCase "0.12" $ read "0.12" @=? fromDouble 0.12,
  testCase "0.012" $ read "0.012" @=? fromDouble 0.012,
  testCase "1234567890123456" $
    read "1234567890123456" @=? fromDouble 1234567890123456,

  testCase "-0" $ read "0" @=? fromDouble (-0),
  testCase "-120" $ read "-120" @=? fromDouble (-120),
  testCase "-12" $ read "-12" @=? fromDouble (-12),
  testCase "-1.2" $ read "-1.2" @=? fromDouble (-1.2),
  testCase "-0.12" $ read "-0.12" @=? fromDouble (-0.12),
  testCase "-0.012" $ read "-0.012" @=? fromDouble (-0.012),
  testCase "-1234567890123456" $
    read "-1234567890123456" @=? fromDouble (-1234567890123456)
  ]

toDoubleTests = testGroup "toDouble" [
  testCase "0" $ 0 @=? toDouble (read "0"),
  testCase "30" $ 30 @=? toDouble (read "30"),
  testCase "0.125" $ 0.125 @=? toDouble (read "0.125"),
  testCase "-1234567890" $ (-1234567890) @=? toDouble (read "-1234567890")
  ]

numInstanceTests = testGroup "Num instance" [
  testGroup "addition" [
    testAddition "2" "5" 7 0,
    testAddition "5" "2" 7 0,
    testAddition "123" "4560" 4683 0,
    testAddition "4560" "123" 4683 0,
    testAddition "1000" "0.01" 100001 (-2),
    testAddition "-120" "120" 0 0,
    testAddition "-120" "119.9" (-1) (-1)
    ],

  testGroup "subtraction" [
    testSubtraction "2" "5" (-3) 0,
    testSubtraction "5" "2" 3 0,
    testSubtraction "123" "4560" (-4437) 0,
    testSubtraction "4560" "123" 4437 0,
    testSubtraction "1000" "0.01" 99999 (-2),
    testSubtraction "119.9" "120" (-1) (-1),
    testSubtraction "120" "120" 0 0
    ],

  testGroup "multiplication" [
    testMultiplication "0" "0" 0 0,
    testMultiplication "0" "1" 0 0,
    testMultiplication "10" "0" 0 0,
    testMultiplication "10000" "0.001" 1 1,
    testMultiplication "10000" "0.0001" 1 0,
    testMultiplication "10000" "0.00001" 1 (-1),
    testMultiplication "123.456" "10000.4" 12346093824 (-4)
    ],

  testGroup "negation" [
    testCase "0" $ assertBigfloat 0 0 $ negate $ read "0",
    testCase "100" $ assertBigfloat (-1) 2 $ negate $ read "100",
    testCase "-4.01" $ assertBigfloat 401 (-2) $ negate $ read "-4.01"
    ],

  testGroup "abs" [
    testCase "0" $ assertBigfloat 0 0 $ abs $ read "0",
    testCase "100" $ assertBigfloat 1 2 $ abs $ read "100",
    testCase "-4.01" $ assertBigfloat 401 (-2) $ abs $ read "-4.01"
    ],

  testGroup "signum" [
    testCase "0" $ assertBigfloat 0 0 $ signum $ read "0",
    testCase "100" $ assertBigfloat 1 0 $ signum $ read "100",
    testCase "-4.01" $ assertBigfloat (-1) 0 $ signum $ read "-4.01"
    ],

  testGroup "fromInteger" [
    testCase "0" $ assertBigfloat 0 0 (0 :: Bigfloat),
    testCase "100" $ assertBigfloat 1 2 (100 :: Bigfloat),
    testCase "-12" $ assertBigfloat (-12) 0 (-12 :: Bigfloat)
    ]
  ]

eqInstanceTests = testGroup "Eq instance" [
  testCase "0 == 0" $ encode 0 0 @=? encode 0 0,
  testCase "1 == 1" $ encode 1 0 @=? encode 1 0,
  testCase "1 == 1.0" $ encode 1 0 @=? encode 10 (-1),
  testCase "-1 == -1" $ encode (-1) 0 @=? encode (-1) 0,

  testCase "0 /= 1" $ encode 0 0 @/=? encode 1 0,
  testCase "1 /= -1" $ encode 1 0 @/=? encode (-1) 0,
  testCase "50 /= 5" $ encode 50 0 @/=? encode 5 0,
  testCase "314 /= 0.314" $ encode 314 0 @/=? encode 314 (-3)
  ]

ordInstanceTests = testGroup "ordering" [
  testCase "0 EQ 0" $ EQ @=? encode 0 0 `compare` encode 0 0,
  testCase "0 LT 5" $ LT @=? encode 0 0 `compare` encode 5 0,
  testCase "-1 LT 5" $ LT @=? encode (-1) 0 `compare` encode 5 0,
  testCase "5 GT 0" $ GT @=? encode 5 0 `compare` encode 0 0,
  testCase "0.123 LT 123" $ LT @=? encode 123 (-3) `compare` encode 123 0
  ]
