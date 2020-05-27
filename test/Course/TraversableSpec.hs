{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.TraversableSpec where


import           Test.Hspec               (Spec, it, shouldBe, describe)

import           Course.Compose     (Compose (Compose))
import           Course.Core
import           Course.ExactlyOne  (ExactlyOne (ExactlyOne))
import           Course.Functor
import           Course.List        (List (Nil), listh)
import           Course.Optional    (Optional (Empty, Full))
import           Course.Traversable


spec :: Spec
spec = do
  describe "List" $ do
    it "traverse on empty list" $
      traverse (\a -> Full (a * 2)) (Nil :: List Int) `shouldBe` Full Nil
    it "traverse on non-empty list" $
      traverse (\a -> Full (a * 2)) (listh [1, 2, 3]) `shouldBe` Full (listh [2, 4, 6])

  describe "ExactlyOne" $
    it "traverse on ExactlyOne" $
      traverse (\a -> Full (a * 2)) (ExactlyOne 3) `shouldBe` Full (ExactlyOne 6)

  describe "optionalTest" $ do
    it "traverse on Empty" $
      traverse (\a -> ExactlyOne (a * 2)) Empty `shouldBe` ExactlyOne Empty
    it "traverse on Full" $
      traverse (\a -> ExactlyOne (a * 2)) (Full 5) `shouldBe` ExactlyOne (Full 10)

  describe "sequenceATest" $ do
    it "on List over ExactlyOne" $
      sequenceA (listh [ExactlyOne 7, ExactlyOne 8, ExactlyOne 9]) `shouldBe` ExactlyOne (listh [7,8,9])
    it "on Optional over ExactlyOne" $
      sequenceA (Full (ExactlyOne 7)) `shouldBe` ExactlyOne (Full 7)
    it "on Optional over function" $
      sequenceA (Full (*10)) 6 `shouldBe` Full 60
    
  describe "composeTest" $ do
    let fmap2 f = ((f <$>) <$>)
        traversedClei = Compose $ (*2) `fmap2` listOfExactlyOnes
        listOfExactlyOnes = listh [ExactlyOne 1, ExactlyOne 2, ExactlyOne 3]
        clei = Compose listOfExactlyOnes
        fullListOfInts = Full (listh [1, 2, 3])
        traversedCfli = Compose $ (*2) `fmap2` fullListOfInts
        cfli = Compose fullListOfInts
    it "traverse on Compose Optional List Int" $
      traverse (\a -> ExactlyOne (a * 2)) cfli `shouldBe` ExactlyOne traversedCfli
    it "traverse on Compose List ExactlyOne Int" $
      traverse (\a -> Full (a * 2)) clei `shouldBe` Full traversedClei

  describe "productFunctorTest" $ do
    let listOfInts = listh [1, 2, 3]
    it "fmap on Product Optional List Int" $
      (*2) <$> Product (Full 4) listOfInts `shouldBe` Product (Full 8) ((*2) <$> listOfInts)
    it "fmap on Product ExactlyOne Optional Int" $
      (*2) <$> Product (ExactlyOne 4) Empty `shouldBe` Product (ExactlyOne 8) Empty

  describe "productTraversableTest" $ do
    let listOfInts = listh [1, 2, 3]
        product = Product (Full 4) listOfInts
        productTimesTwo = Product (Full 8) ((*2) <$> listOfInts)
    it "traverse on Product Optional List Int" $
      traverse (\a -> ExactlyOne (a*2)) product `shouldBe` ExactlyOne productTimesTwo

  describe "coProductFunctorTest" $ do
    let inL, inLTimesTwo :: Coproduct Optional List Int
        inL = InL (Full 4)
        inLTimesTwo = InL (Full 8)
        inR, inRTimesTwo :: Coproduct Optional List Int
        inR = InR listOfInts
        inRTimesTwo = InR ((*2) <$> listOfInts)
        listOfInts = listh [1, 2, 3]
    it "fmap on InL Optional Int" $
      (*2) <$> inL `shouldBe` inLTimesTwo
    it "fmap on InR ExactlyOne Int" $
      (*2) <$> inR `shouldBe` inRTimesTwo

  describe "coProductTraversableTest" $ do
    let inL, inLTimesTwo :: Coproduct Optional List Int
        inL = InL (Full 4)
        inLTimesTwo = InL (Full 8)
        inR, inRTimesTwo :: Coproduct Optional List Int
        inR = InR listOfInts
        inRTimesTwo = InR ((*2) <$> listOfInts)
        listOfInts = listh [1, 2, 3]
    it "traverse on InL Optional Int" $
      traverse (\a -> ExactlyOne (a*2)) inL `shouldBe` ExactlyOne inLTimesTwo
    it "traverse on InR List Int" $
      traverse (\a -> Full (a*2)) inR `shouldBe` Full inRTimesTwo
