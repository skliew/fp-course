{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ChecqueSpec where

import           Test.Hspec            (describe, it, Spec, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       ((===))

import           Course.Core
import           Course.Cheque (dollars)
import           Course.List       (List (..))

spec :: Spec
spec = chequeDollars

chequeDollars :: Spec
chequeDollars =
  describe "dollars" $ do
    it "empty" $ do
      dollars "0" `shouldBe` "zero dollars and zero cents"
    it "dollars '1'" $
      dollars "1" `shouldBe` "one dollar and zero cents"
    it "dollars '0.1'" $
      dollars "0.1" `shouldBe` "zero dollars and ten cents"
    it "dollars '1.'" $
      dollars "1." `shouldBe` "one dollar and zero cents"
    it "dollars '0.'" $
      dollars "0." `shouldBe` "zero dollars and zero cents"
    it "dollars '0.0'" $
      dollars "0.0" `shouldBe` "zero dollars and zero cents"
    it "dollars '.34'" $
      dollars ".34" `shouldBe` "zero dollars and thirty-four cents"
    it "dollars '0.3456789'" $
      dollars "0.3456789" `shouldBe` "zero dollars and thirty-four cents"
    it "dollars '1.0'" $
      dollars "1.0" `shouldBe` "one dollar and zero cents"
    it "dollars '1.01'" $
      dollars "1.01" `shouldBe` "one dollar and one cent"
    it "dollars 'a1a'" $
      dollars "a1a" `shouldBe` "one dollar and zero cents"
    it "dollars 'a1a.a0.7b'" $
      dollars "a1a.a0.7b" `shouldBe` "one dollar and seven cents"
    it "dollars '100'" $
      dollars "100" `shouldBe` "one hundred dollars and zero cents"
    it "dollars '100.0'" $
      dollars "100.0" `shouldBe` "one hundred dollars and zero cents"
    it "dollars '100.00'" $
      dollars "100.00" `shouldBe` "one hundred dollars and zero cents"
    it "dollars '100.00000'" $
      dollars "100.00000" `shouldBe` "one hundred dollars and zero cents"
    it "dollars '1000456.13'" $
      dollars "1000456.13" `shouldBe` "one million four hundred and fifty-six dollars and thirteen cents"
    it "dollars '1001456.13'" $
      dollars "1001456.13" `shouldBe` "one million one thousand four hundred and fifty-six dollars and thirteen cents"
    it "dollars '16000000456.13'" $
      dollars "16000000456.13" `shouldBe` "sixteen billion four hundred and fifty-six dollars and thirteen cents"
    it "dollars '100.45'" $
      dollars "100.45" `shouldBe` "one hundred dollars and forty-five cents"
    it "dollars '100.07'" $
      dollars "100.07" `shouldBe` "one hundred dollars and seven cents"
    it "dollars '9abc9def9ghi.jkl9mno'" $
      dollars "9abc9def9ghi.jkl9mno" `shouldBe` "nine hundred and ninety-nine dollars and ninety cents"
    it "dollars '12345.67'" $
      dollars "12345.67" `shouldBe` "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
    it "dollars '456789123456789012345678901234567890123456789012345678901234567890.12'" $
      dollars "456789123456789012345678901234567890123456789012345678901234567890.12" `shouldBe` "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
