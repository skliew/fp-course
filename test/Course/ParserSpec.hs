{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ParserSpec where

import           Test.Hspec            (describe, it, Spec, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       ((===))

import           Course.Applicative (pure, (*>), (<*>))
import           Course.Core
import           Course.Functor     ((<$>))
import           Course.List        (List ((:.), Nil))
import           Course.Monad       ((=<<))
import           Course.Optional    (Optional (Full))
import           Course.Parser
import           Course.Person      (Person (Person))

spec :: Spec
spec = do
  describe "characterTest" $ do
      it "parses single character from non-empty string" $
        parse character "abc" `shouldBe` Result "bc" 'a'
      it "parsing empty string is an error" $
        isErrorResult (parse character "") `shouldBe` True

  describe "functorTest" $ do
    it "toUpper <$>" $
      parse (toUpper <$> character) "amz" `shouldBe` Result "mz" 'A'

  describe "valueParserTest" $ do
    it "succeeds with given value" $
      parse (valueParser 3) "abc" `shouldBe` Result "abc" 3

  describe "alternativeParserTest" $ do
    it "first fails, second succeeds with no input" $
      parse (character ||| valueParser 'v') "" `shouldBe` Result "" 'v'
    it "first always fails, second succeeds with no input" $
      parse (constantParser UnexpectedEof ||| valueParser 'v') "" `shouldBe` Result "" 'v'
    it "first always fails, second succeeds with input" $
      parse (constantParser UnexpectedEof ||| valueParser 'v') "abc" `shouldBe` Result "abc" 'v'
    it "takes first parse result when it succeeds" $
      parse (character ||| valueParser 'v') "abc" `shouldBe` Result "bc" 'a'

  describe "parserMonadInstanceTest" $ do
    it "first parse fails" $
      isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "")
    it "second parse fails" $
      isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "x")
    it "bind to valueParser" $
      parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "abc" `shouldBe` Result "bc" 'v'
    it "bind to valueParser with no more input" $
      parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "a" `shouldBe` Result "" 'v'
    it "bind to character parser with remaining input" $
      parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "xabc" `shouldBe` Result "bc" 'a'

  describe "parserApplicativeInstanceTest" $ do
    it "pure" $
      parse (pure 'a' :: Parser Char) "xyz" `shouldBe` Result "xyz" 'a'
    it "pure an Optional value" $
      parse (pure (Full 5) :: Parser (Optional Int)) "xyz" `shouldBe` Result "xyz" (Full 5)
    it "pure toUpper <*>" $
      parse (pure toUpper <*> valueParser 'a') "xyz" `shouldBe` Result "xyz" 'A'
    it "pure show <*>" $
      parse (pure show <*> valueParser 599) "xyz" `shouldBe` Result "xyz" "599"
    it "append character <*>" $
      parse (((\a b -> a :. b :. Nil) <$> character) <*> character) "abxyz" `shouldBe` Result "xyz" "ab"

  describe "listTest" $ do
    it "succeeds on empty input" $
      parse (list character) "" `shouldBe` Result "" ""
    it "parses for as long as characters match" $
      parse (list digit) "123abc" `shouldBe` Result "abc" "123"
    it "parses empty value when no matching characters" $
      parse (list digit) "abc" `shouldBe` Result "abc" ""
    it "parses entire input if matches" $
      parse (list character) "abc" `shouldBe` Result "" "abc"
    it "parses for as long as characters match" $
      parse (list (character *> valueParser 'v')) "abc" `shouldBe` Result "" "vvv"
    it "succeeds on empty input with value parser" $
        parse (list (character *> valueParser 'v')) "" `shouldBe` Result "" ""

  describe "list1Test" $ do
    it "succeeds when at least one character matches" $
      parse (list1 character) "abc" `shouldBe`  Result "" "abc"
    it "succeeds when at least one character matches" $
      parse (list1 (character *> valueParser 'v')) "abc"  `shouldBe` Result "" "vvv"
    it "no matching chars fails" $
      isErrorResult (parse (list1 (character *> valueParser 'v')) "")

  describe "spaces1Test" $ do
    it "fails on empty string" $
      isErrorResult (parse spaces1 "")
    it "consumes single space" $
      parse spaces1 " " `shouldBe` Result "" " "
    it "consumes multiple spaces" $
      parse spaces1 "    abc" `shouldBe` Result "abc" "    "

  describe "lowerTest" $ do
    it "fails on empty string" $
      isErrorResult (parse lower "")
    it "fails if character is not lowercase" $
      isErrorResult (parse lower "Abc")
    it "produces lowercase character" $
      parse lower "aBC" `shouldBe` Result "BC" 'a'

  describe "upperTest" $ do
    it "fails on empty string" $
      isErrorResult (parse upper "")
    it "fails if character is not uppercase" $
      isErrorResult (parse upper "aBC")
    it "produces uppercase character" $
      parse upper "Abc" `shouldBe` Result "bc" 'A'

  describe "alphaTest" $ do
    it "fails on empty string" $
      isErrorResult (parse alpha "")
    it "fails if character is not alpha" $
      isErrorResult (parse alpha "5BC")
    it "produces alpha character" $
      parse alpha "A45" `shouldBe` Result "45" 'A'

  describe "sequenceParserTest" $ do
    it "fails on first failing parser" $
      isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
    it "sequences list of successful parsers" $
      parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef" `shouldBe` Result "def" "axC"

  describe "thisManyTest" $ do
    it "fails when not enough matches" $
      isErrorResult (parse (thisMany 4 upper) "ABcDef")
    it "produces n values when matched" $
      parse (thisMany 4 upper) "ABCDef" `shouldBe` Result "ef" "ABCD"

  describe "ageParserTest (done for you)" $ do
    it "fails on invalid age (all letters)" $
        isErrorResult (parse ageParser "abc")
    it "fails on invalid age (leading '-')" $
      isErrorResult (parse ageParser "-120")
    it "parses valid age" $
      parse ageParser "120" `shouldBe` Result "" 120

  describe "firstNameParserTest" $ do
    it "fails on first name that doesn't start with a capital" $
      isErrorResult (parse firstNameParser "abc")
    it "parses valid first name" $
      parse firstNameParser "Abc" `shouldBe` Result "" "Abc"

  describe "surnameParserTest" $ do
    it "fails on short surname" $
      isErrorResult (parse surnameParser "Abc")
    it "fails on short surname starting with a lower case letter" $
      isErrorResult (parse surnameParser "abc")
    it "parses shortest valid surname" $
      parse surnameParser "Abcdef" `shouldBe` Result "" "Abcdef"
    it "parses long surname" $
      parse surnameParser "Abcdefghijklmnopqrstuvwxyz" `shouldBe` Result "" "Abcdefghijklmnopqrstuvwxyz"

  describe "smokerParserTest" $ do
    it "fails on non y/n value" $
      isErrorResult (parse smokerParser "abc")
    it "parses y, leaving remaining input" $
      parse smokerParser "yabc" `shouldBe` Result "abc" True
    it "parses n, leaving remaining input" $
      parse smokerParser "nabc" `shouldBe` Result "abc" False

  describe "phoneBodyParserTest" $ do
    it "produces empty list when no characters match" $
      parse phoneBodyParser "a123-456" `shouldBe` Result "a123-456" ""
    it "parses valid phone body value" $
      parse phoneBodyParser "123-456" `shouldBe` Result "" "123-456"
    it "parses up to first letter" $
      parse phoneBodyParser "123-a456" `shouldBe` Result "a456" "123-"

  describe "phoneParserTest" $ do
    it "fails without trailing '#'" $
      isErrorResult (parse phoneParser "123-456")
    it "fails when input starts with a letter" $
      isErrorResult (parse phoneParser "a123-456")
    it "produces valid phone number" $
      parse phoneParser "123-456#" `shouldBe` Result "" "123-456"
    it "produces a valid phone number with remaining input" $
      parse phoneParser "123-456#abc" `shouldBe` Result "abc" "123-456"

  describe "personParserTest" $ do
    it "fails on empty string" $
      isErrorResult (parse personParser "")
    it "fails on invalid age" $
      isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
    it "fails on first name that doesn't start with capital" $
      isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
    it "fails on surname that is too short" $
      isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
    it "fails on surname that doesn't start with a capital letter" $
      isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
    it "fails on invalid smoker value 'x'" $
      isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
    it "fails on phone number containing an 'x'" $
      isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
    it "fails on phone number starting with '-'" $
      isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
    it "fails on phone number without a trailing '#'" $
      isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
    it "produces person for valid input" $
      parse personParser "123 Fred Clarkson y 123-456.789#" `shouldBe`
        Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
    it "produces person for valid input and keeps remaining input" $
      parse personParser "123 Fred Clarkson y 123-456.789# rest" `shouldBe`
        Result " rest" (Person 123 "Fred" "Clarkson" True "123-456.789")
    it "produces person for valid input containing extra whitespace" $
      parse personParser "123  Fred   Clarkson    y     123-456.789#" `shouldBe`
        Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
