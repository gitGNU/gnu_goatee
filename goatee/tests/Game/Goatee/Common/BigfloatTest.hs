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
import Test.HUnit ((~:), (@=?), Assertion, Test (TestList))

assertBigfloat :: Integer -> Int -> Bigfloat -> Assertion
assertBigfloat v e x =
  (v, e) @=? (significand x, exponent x)

testAddition :: String -> String -> Integer -> Int -> Test
testAddition x y v e =
  x ++ " + " ++ y ~: assertBigfloat v e $ read x + read y

testSubtraction :: String -> String -> Integer -> Int -> Test
testSubtraction x y v e =
  x ++ " - " ++ y ~: assertBigfloat v e $ read x - read y

testMultiplication :: String -> String -> Integer -> Int -> Test
testMultiplication x y v e =
  x ++ " * " ++ y ~: assertBigfloat v e $ read x * read y

tests = "Game.Goatee.Common.Bigfloat" ~: TestList
  [ encodeTests
  , readInstanceTests
  , showInstanceTests
  , fromDoubleTests
  , toDoubleTests
  , numInstanceTests
  , eqInstanceTests
  , ordInstanceTests
  ]

encodeTests = "encode" ~: TestList
  [ "zero" ~: assertBigfloat 0 0 $ encode 0 0

  , "whole numbers" ~: TestList
    [ "1" ~: assertBigfloat 1 0 $ encode 1 0
    , "5" ~: assertBigfloat 5 0 $ encode 5 0
    , "12" ~: assertBigfloat 12 0 $ encode 12 0
    , "1031" ~: assertBigfloat 1031 0 $ encode 1031 0

    , "-1" ~: assertBigfloat (-1) 0 $ encode (-1) 0
    , "-5" ~: assertBigfloat (-5) 0 $ encode (-5) 0
    , "-12" ~: assertBigfloat (-12) 0 $ encode (-12) 0
    , "-1031" ~: assertBigfloat (-1031) 0 $ encode (-1031) 0
    ]

  , "fractional numbers" ~: TestList
    [ "0.1" ~: assertBigfloat 1 (-1) $ encode 1 (-1)
    , "0.5" ~: assertBigfloat 5 (-1) $ encode 5 (-1)
    , "1.5" ~: assertBigfloat 15 (-1) $ encode 15 (-1)
    , "123.456" ~: assertBigfloat 123456 (-3) $ encode 123456 (-3)
    , "0.00125" ~: assertBigfloat 125 (-5) $ encode 125 (-5)

    , "-0.1" ~: assertBigfloat (-1) (-1) $ encode (-1) (-1)
    , "-0.5" ~: assertBigfloat (-5) (-1) $ encode (-5) (-1)
    , "-1.5" ~: assertBigfloat (-15) (-1) $ encode (-15) (-1)
    , "-123.456" ~: assertBigfloat (-123456) (-3) $ encode (-123456) (-3)
    , "-0.00125" ~: assertBigfloat (-125) (-5) $ encode (-125) (-5)
    ]

  , "extreme numbers" ~: TestList
    [ "12345678901234567890123456789012345678901234567890" ~:
      assertBigfloat 1234567890123456789012345678901234567890123456789 1 $
      encode 12345678901234567890123456789012345678901234567890 0

    , "0.00000000000000000000000000000000000000000000000001" ~:
      assertBigfloat 1 (-50) $ encode 1 (-50)

    , "123456789012345678901234567890.123456789012345678901234567890" ~:
      assertBigfloat 12345678901234567890123456789012345678901234567890123456789 (-29) $
      encode 123456789012345678901234567890123456789012345678901234567890 (-30)
    ]

  , "reduction" ~: TestList
    [ "50.0" ~: assertBigfloat 5 1 $ encode 500 (-1)
    , "123.456000" ~: assertBigfloat 123456 (-3) $ encode 123456000 (-6)

    , "-50.0" ~: assertBigfloat (-5) 1 $ encode (-500) (-1)
    , "-123.456000" ~: assertBigfloat (-123456) (-3) $ encode (-123456000) (-6)
    ]
  ]

readInstanceTests = "Read instance" ~: TestList
  [ "0" ~: assertBigfloat 0 0 $ read "0"
  , "3" ~: assertBigfloat 3 0 $ read "3"
  , "10" ~: assertBigfloat 1 1 $ read "10"
  , "51200" ~: assertBigfloat 512 2 $ read "51200"
  , "5120" ~: assertBigfloat 512 1 $ read "5120"
  , "512" ~: assertBigfloat 512 0 $ read "512"
  , "512.0" ~: assertBigfloat 512 0 $ read "512.0"
  , "51.2" ~: assertBigfloat 512 (-1) $ read "51.2"
  , "05.12" ~: assertBigfloat 512 (-2) $ read "05.12"
  , "0.512" ~: assertBigfloat 512 (-3) $ read "0.512"
  , "0.051200" ~: assertBigfloat 512 (-4) $ read "0.051200"

  , "-0" ~: assertBigfloat 0 0 $ read "-0"
  , "-3" ~: assertBigfloat (-3) 0 $ read "-3"
  , "-10" ~: assertBigfloat (-1) 1 $ read "-10"
  , "-51200" ~: assertBigfloat (-512) 2 $ read "-51200"
  , "-5120" ~: assertBigfloat (-512) 1 $ read "-5120"
  , "-512" ~: assertBigfloat (-512) 0 $ read "-512"
  , "-512.0" ~: assertBigfloat (-512) 0 $ read "-512.0"
  , "-51.2" ~: assertBigfloat (-512) (-1) $ read "-51.2"
  , "-05.12" ~: assertBigfloat (-512) (-2) $ read "-05.12"
  , "-0.512" ~: assertBigfloat (-512) (-3) $ read "-0.512"
  , "-0.051200" ~: assertBigfloat (-512) (-4) $ read "-0.051200"

  , "0e0" ~: assertBigfloat 0 0 $ read "0e0"
  , "0e-0" ~: assertBigfloat 0 0 $ read "0e-0"
  , "0e1" ~: assertBigfloat 0 0 $ read "0e1"
  , "0e-1" ~: assertBigfloat 0 0 $ read "0e-1"
  , "0.0e0" ~: assertBigfloat 0 0 $ read "0.0e0"
  , "1e0" ~: assertBigfloat 1 0 $ read "1e0"
  , "1e1" ~: assertBigfloat 1 1 $ read "1e1"
  , "1e2" ~: assertBigfloat 1 2 $ read "1e2"
  , "1e-1" ~: assertBigfloat 1 (-1) $ read "1e-1"
  , "1e-2" ~: assertBigfloat 1 (-2) $ read "1e-2"
  , "100e2" ~: assertBigfloat 1 4 $ read "100e2"
  , "100e-1" ~: assertBigfloat 1 1 $ read "100e-1"
  , "100e-3" ~: assertBigfloat 1 (-1) $ read "100e-3"
  , "0.0002e-5" ~: assertBigfloat 2 (-9) $ read "0.0002e-5"
  , "0.0002e2" ~: assertBigfloat 2 (-2) $ read "0.0002e2"
  , "0.0002e5" ~: assertBigfloat 2 1 $ read "0.0002e5"
  ]

showInstanceTests = "Show instance" ~: TestList
  [ "0" ~: "0" @=? show (encode 0 0)

  , "3" ~: "3" @=? show (encode 3 0)
  , "10" ~: "10" @=? show (encode 1 1)
  , "51200" ~: "51200" @=? show (encode 512 2)
  , "5120" ~: "5120" @=? show (encode 512 1)
  , "512" ~: "512" @=? show (encode 512 0)
  , "51.2" ~: "51.2" @=? show (encode 512 (-1))
  , "5.12" ~: "5.12" @=? show (encode 512 (-2))
  , "0.512" ~: "0.512" @=? show (encode 512 (-3))
  , "0.0512" ~: "0.0512" @=? show (encode 512 (-4))

  , "-3" ~: "-3" @=? show (encode (-3) 0)
  , "-10" ~: "-10" @=? show (encode (-10) 0)
  , "-51200" ~: "-51200" @=? show (encode (-512) 2)
  , "-5120" ~: "-5120" @=? show (encode (-512) 1)
  , "-512" ~: "-512" @=? show (encode (-512) 0)
  , "-51.2" ~: "-51.2" @=? show (encode (-512) (-1))
  , "-5.12" ~: "-5.12" @=? show (encode (-512) (-2))
  , "-0.512" ~: "-0.512" @=? show (encode (-512) (-3))
  , "-0.0512" ~: "-0.0512" @=? show (encode (-512) (-4))
  ]

fromDoubleTests = "fromDouble" ~: TestList
  [ "0" ~: read "0" @=? fromDouble 0
  , "120" ~: read "120" @=? fromDouble 120
  , "12" ~: read "12" @=? fromDouble 12
  , "1.2" ~: read "1.2" @=? fromDouble 1.2
  , "0.12" ~: read "0.12" @=? fromDouble 0.12
  , "0.012" ~: read "0.012" @=? fromDouble 0.012
  , "1234567890123456" ~: read "1234567890123456" @=? fromDouble 1234567890123456

  , "-0" ~: read "0" @=? fromDouble (-0)
  , "-120" ~: read "-120" @=? fromDouble (-120)
  , "-12" ~: read "-12" @=? fromDouble (-12)
  , "-1.2" ~: read "-1.2" @=? fromDouble (-1.2)
  , "-0.12" ~: read "-0.12" @=? fromDouble (-0.12)
  , "-0.012" ~: read "-0.012" @=? fromDouble (-0.012)
  , "-1234567890123456" ~: read "-1234567890123456" @=? fromDouble (-1234567890123456)
  ]

toDoubleTests = "toDouble" ~: TestList
  [ "0" ~: 0 @=? toDouble (read "0")
  , "30" ~: 30 @=? toDouble (read "30")
  , "0.125" ~: 0.125 @=? toDouble (read "0.125")
  , "-1234567890" ~: (-1234567890) @=? toDouble (read "-1234567890")
  ]

numInstanceTests = "Num instance" ~: TestList
  [ "addition" ~: TestList
    [ testAddition "2" "5" 7 0
    , testAddition "5" "2" 7 0
    , testAddition "123" "4560" 4683 0
    , testAddition "4560" "123" 4683 0
    , testAddition "1000" "0.01" 100001 (-2)
    , testAddition "-120" "120" 0 0
    , testAddition "-120" "119.9" (-1) (-1)
    ]

  , "subtraction" ~: TestList
    [ testSubtraction "2" "5" (-3) 0
    , testSubtraction "5" "2" 3 0
    , testSubtraction "123" "4560" (-4437) 0
    , testSubtraction "4560" "123" 4437 0
    , testSubtraction "1000" "0.01" 99999 (-2)
    , testSubtraction "119.9" "120" (-1) (-1)
    , testSubtraction "120" "120" 0 0
    ]

  , "multiplication" ~: TestList
    [ testMultiplication "0" "0" 0 0
    , testMultiplication "0" "1" 0 0
    , testMultiplication "10" "0" 0 0
    , testMultiplication "10000" "0.001" 1 1
    , testMultiplication "10000" "0.0001" 1 0
    , testMultiplication "10000" "0.00001" 1 (-1)
    , testMultiplication "123.456" "10000.4" 12346093824 (-4)
    ]

  , "negation" ~: TestList
    [ "0" ~: assertBigfloat 0 0 $ negate $ read "0"
    , "100" ~: assertBigfloat (-1) 2 $ negate $ read "100"
    , "-4.01" ~: assertBigfloat 401 (-2) $ negate $ read "-4.01"
    ]

  , "abs" ~: TestList
    [ "0" ~: assertBigfloat 0 0 $ abs $ read "0"
    , "100" ~: assertBigfloat 1 2 $ abs $ read "100"
    , "-4.01" ~: assertBigfloat 401 (-2) $ abs $ read "-4.01"
    ]

  , "signum" ~: TestList
    [ "0" ~: assertBigfloat 0 0 $ signum $ read "0"
    , "100" ~: assertBigfloat 1 0 $ signum $ read "100"
    , "-4.01" ~: assertBigfloat (-1) 0 $ signum $ read "-4.01"
    ]

  , "fromInteger" ~: TestList
    [ "0" ~: assertBigfloat 0 0 (0 :: Bigfloat)
    , "100" ~: assertBigfloat 1 2 (100 :: Bigfloat)
    , "-12" ~: assertBigfloat (-12) 0 (-12 :: Bigfloat)
    ]
  ]

eqInstanceTests = "Eq instance" ~: TestList
  [ "0 == 0" ~: encode 0 0 @=? encode 0 0
  , "1 == 1" ~: encode 1 0 @=? encode 1 0
  , "1 == 1.0" ~: encode 1 0 @=? encode 10 (-1)
  , "-1 == -1" ~: encode (-1) 0 @=? encode (-1) 0

  , "0 /= 1" ~: encode 0 0 @/=? encode 1 0
  , "1 /= -1" ~: encode 1 0 @/=? encode (-1) 0
  , "50 /= 5" ~: encode 50 0 @/=? encode 5 0
  , "314 /= 0.314" ~: encode 314 0 @/=? encode 314 (-3)
  ]

ordInstanceTests = "ordering" ~: TestList
  [ "0 EQ 0" ~: EQ @=? encode 0 0 `compare` encode 0 0
  , "0 LT 5" ~: LT @=? encode 0 0 `compare` encode 5 0
  , "-1 LT 5" ~: LT @=? encode (-1) 0 `compare` encode 5 0
  , "5 GT 0" ~: GT @=? encode 5 0 `compare` encode 0 0
  , "0.123 LT 123" ~: LT @=? encode 123 (-3) `compare` encode 123 0
  ]
