module Main (main) where

--------------------------------------------------------------------------------

tryLoadPill :: PillFile -> IO Atom
tryLoadPill pill = do
    a@(MkAtom nat) <- loadAtom (show pill)
    putStrLn "loaded"
    print (a > 0)
    putStrLn "evaled"
    print (take 10 $ VP.toList $ nat ^. natWords)
    pure a

tryPackPill :: PillFile -> IO ()
tryPackPill pf = do
  atm <- tryLoadPill pf
  print $ length (atm ^. pill . pillBS)


-- Tests -----------------------------------------------------------------------

instance Arbitrary ByteString where
  arbitrary = fromList <$> arbitrary

instance Arbitrary Pill where
  arbitrary = Pill <$> arbitrary

instance Arbitrary BigNat where
  arbitrary = view naturalBigNat <$> arbitrary

instance Show BigNat where
  show = show . NatJ#

--------------------------------------------------------------------------------

testIso :: Eq a => Iso' a b -> a -> Bool
testIso iso x = x == (x ^. iso . from iso)

roundTrip :: Eq a => (a -> b) -> (b -> a) -> (a -> Bool)
roundTrip dump load x = x == load (dump x)

equiv :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
equiv f g x = f x == g x

check :: Atom -> Atom
check = toAtom . (id :: Integer -> Integer) . fromAtom

--------------------------------------------------------------------------------

prop_packWordSane = equiv (view packedWord) dumbPackWord . fromList
prop_packWord     = testIso (from packedWord)
prop_unpackWord   = roundTrip (view packedWord)
                              (strip . view (from packedWord))
                  . strip
                  . take 8

prop_unpackBigNat = testIso bigNatWords

prop_packBigNat   = roundTrip (view (from bigNatWords) . VP.fromList)
                              (strip . VP.toList . view bigNatWords)
                  . strip

prop_implodeBytes = roundTrip (view pillWords) (view (from pillWords))

prop_explodeBytes = roundTrip (view (from pillWords) . VP.fromList)
                              (strip . VP.toList . view pillWords)
                  . strip

prop_packAtomSane = equiv (view (from pill)) dumbPackAtom . Pill . fromList
prop_unpackAtom   = roundTrip (view pill) (view (from pill))
prop_packAtom     = roundTrip (view (from pill)) (view pill) . Pill . strip


--------------------------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)
