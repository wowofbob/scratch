{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

class Supply s f a where
  supply :: f -> s -> (a, s)

newtype Gen = Gen (Int, Char)

data A = A
data B = B

instance Supply Gen A Int where
  supply _ (Gen (i, c)) = (i, Gen (i + 1, c))

instance Supply Gen B Char where
  supply _ (Gen (i, c)) = (c, Gen (i, succ c))

makeA :: Gen -> (Int, Gen, A)
makeA g =
  let (i, g') = supply A g :: (Int, Gen)
    in (i, g', A)

makeB :: Gen -> (Char, Gen, B)
makeB g =
  let (c, g') = supply B g :: (Char, Gen)
    in (c, g', B)

main :: IO ()
main = pure ()
