module Khumba.GoHS.Common where

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft failed."

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight failed."
