{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.MoreParserSpec where

import           Test.Hspec        (Spec, describe, it, shouldBe)

import           Course.Core
import           Course.List       (List ((:.), Nil))
import           Course.MoreParser (between, betweenCharTok, betweenSepbyComma,
                                    charTok, commaTok, digits1, eof, hex, hexu,
                                    noneof, oneof, option, quote, satisfyAll,
                                    satisfyAny, sepby, sepby1, spaces, string,
                                    stringTok, tok)
import           Course.Parser     (ParseResult (Result), character, is,
                                    isErrorResult, lower, parse)

spec :: Spec
spec = do
  describe "spacesTest" $ do
    it "can parse zero spaces" $
      parse spaces "abc" `shouldBe` Result "abc" ""
    it "can parse single space" $
      parse spaces " abc" `shouldBe` Result "abc" " "
    it "can parse multiple spaces" $
      parse spaces "   abc" `shouldBe` Result "abc" "   "

  describe "tokTest" $ do
    it "can parse input without spaces" $
      parse (tok (is 'a')) "abc" `shouldBe` Result "bc" 'a'
    it "can parse single space" $
      parse (tok (is 'a')) "a bc" `shouldBe` Result "bc" 'a'
    it "can parse multiple spaces" $
      parse (tok (is 'a')) "a   bc" `shouldBe` Result "bc" 'a'

  describe "charTokTest" $ do
    it "fails when character does not match" $
      isErrorResult (parse (charTok 'a') "dabc")
    it "parses matching character" $
      parse (charTok 'a') "abc" `shouldBe` Result "bc" 'a'
    it "parses matching character, dropping space" $
      parse (charTok 'a') "a bc" `shouldBe` Result "bc" 'a'
    it "parses matching character, dropping spaces" $
      parse (charTok 'a') "a   bc" `shouldBe` Result "bc" 'a'

  describe "commaTokTest" $ do
    it "fails when character is not a comma" $
      isErrorResult (parse commaTok "1,23")
    it "parses leading comma" $
      parse commaTok ",123" `shouldBe` Result "123" ','
    it "parses leading comma, dropping space" $
      parse commaTok ", 123" `shouldBe` Result "123" ','
    it "parses leading comma, dropping multiple spaces" $
      parse commaTok ",   123" `shouldBe` Result "123" ','

  describe "quoteTest" $ do
    it "fails when character is not a single or double quote" $
      isErrorResult (parse quote "abc")
    it "parses single quote" $
      parse quote "'abc" `shouldBe` Result "abc" '\''
    it "parses double quote" $
      parse quote "\"abc" `shouldBe` Result "abc" '"'

  describe "stringTest" $ do
    it "fails when string is not matched" $
      isErrorResult (parse (string "abc") "bcdef")
    it "parses matching string, leaves remaining input" $
      parse (string "abc") "abcdef" `shouldBe` Result "def" "abc"
    it "parses matching string" $
      parse (string "abc") "abc" `shouldBe` Result "" "abc"

  describe "stringTokTest" $ do
    it "fails when string is not matched" $
      isErrorResult (parse (stringTok "abc") "bc  ")
    it "parses matching string followed by zero spaces" $
      parse (stringTok "abc") "abc" `shouldBe` Result "" "abc"
    it "parses matching string followed by many spaces" $
      parse (stringTok "abc") "abc  " `shouldBe` Result "" "abc"

  describe "optionTest" $ do
    it "produces parsed value when parser succeeds" $
      parse (option 'x' character) "abc" `shouldBe` Result "bc" 'a'
    it "produces given value when parser fails" $
      parse (option 'x' character) "" `shouldBe` Result "" 'x'

  describe "digits1Test" $ do
    it "fails when no digits at start of input" $
      isErrorResult (parse digits1 "abc123")
    it "succeeds on digits" $
      parse digits1 "123" `shouldBe` Result "" "123"
    it "succeeds on digits, leaves remaining input" $
      parse digits1 "123abc" `shouldBe` Result "abc" "123"

  describe "oneofTest" $ do
    it "fails when given character not in string" $
      isErrorResult (parse (oneof "abc") "def")
    it "given character prefixes input" $
      parse (oneof "abc") "bcdef" `shouldBe` Result "cdef" 'b'

  describe "noneofTest" $ do
    it "fails when one of given characters prefixes input" $
      isErrorResult (parse (noneof "abcd") "abc")
    it "succeeds when none of the given characters in input" $
      parse (noneof "xyz") "abc" `shouldBe` Result "bc" 'a'
    it "succeeds when none of the given characters prefixes input" $
      parse (noneof "bcd") "abc" `shouldBe` Result "bc" 'a'

  describe "betweenTest" $ do
    it "fails when opening parse fails" $
      isErrorResult (parse (between (is '[') (is ']') character) "abc]")
    it "fails when surrounded parser fails" $
      isErrorResult (parse (between (is '[') (is ']') character) "[abc]")
    it "fails when closing parse fails" $
      isErrorResult (parse (between (is '[') (is ']') character) "[abc")
    it "succeeds: character surrounded by []'" $
      parse (between (is '[') (is ']') character) "[a]" `shouldBe` Result "" 'a'
    it "succeeds: digits surrounded by []" $
      parse (between (is '[') (is ']') digits1) "[123]" `shouldBe` Result "" "123"

  describe "betweenCharTokTest" $ do
    it "fails when opening character not present" $
      isErrorResult (parse (betweenCharTok '[' ']' character) "abc]")
    it "fails when closing character not present" $
      isErrorResult (parse (betweenCharTok '[' ']' character) "[abc")
    it "fails when surrounded parser fails" $
      isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]")
    it "succeeds: character" $
      parse (betweenCharTok '[' ']' character) "[a]" `shouldBe` Result "" 'a'
    it "succeeds: digits1" $
      parse (betweenCharTok '[' ']' digits1) "[123]" `shouldBe` Result "" "123"

  describe "hexTest" $ do
    it "fails on invalid hex string --- too short" $
      isErrorResult (parse hex "001")
    it "fails on invalid hex string --- invalid char (x)" $
      isErrorResult (parse hex "0axf")
    it "succeeds on valid hex value" $
      parse hex "0010" `shouldBe` Result "" '\DLE'

  describe "hexuTest" $ do
    it "fails when no u at start" $
      isErrorResult (parse hexu "0010")
    it "fails when not 4 hex digits after u" $
      isErrorResult (parse hexu "u010")
    it "fails on invalid hex digit" $
      isErrorResult (parse hexu "u0axf")
    it "succeeds on valid input --- u0010" $
      parse hexu "u0010" `shouldBe` Result "" '\DLE'
    it "succeeds on valid input --- u0a1f" $
      parse hexu "u0a1f" `shouldBe` Result "" '\2591'

  describe "sepby1Test" $ do
    it "fails when first parser fails" $
      isErrorResult (parse (sepby1 character (is ',')) "")
    it "parses single character not followed by seperator" $
      parse (sepby1 character (is ',')) "a" `shouldBe` Result "" "a"
    it "parses multiple matches with separators" $
      parse (sepby1 character (is ',')) "a,b,c" `shouldBe` Result "" "abc"
    it "succeeds until two separators" $
      parse (sepby1 character (is ',')) "a,b,c,,def" `shouldBe` Result "def" "abc,"

  describe "sepbyTest" $ do
    it "succeeds on empty string" $
      parse (sepby character (is ',')) "" `shouldBe` Result "" ""
    it "succeeds on single match without seperator" $
      parse (sepby character (is ',')) "a" `shouldBe` Result "" "a"
    it "succeeds on multiple matches with seperators" $
      parse (sepby character (is ',')) "a,b,c" `shouldBe` Result "" "abc"
    it "succeeds until two separators" $
      parse (sepby character (is ',')) "a,b,c,,def" `shouldBe` Result "def" "abc,"

  describe "eofTest" $ do
    it "fails when still input left" $
      isErrorResult (parse eof "abc")
    it "succeeds when no input left" $
      parse eof "" `shouldBe` Result "" ()

  describe "satisfyAllTest" $ do
    it "fails when a predicate fails" $
      isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "XBc")
    it "fails when no predicates satisfied (empty input)" $
      isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "")
    it "fails when no predicates satisfied" $
      isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "abc")
    it "succeeds when all predicates satisfied: ABC" $
      parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABC" `shouldBe` Result "BC" 'A'
    it "succeeds when all predicates satisfied: ABc" $
      parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABc" `shouldBe` Result "Bc" 'A'

  describe "satisfyAnyTest" $ do
    it "fails when no predicates satisfied" $
      isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "XBc")
    it "fails when no predicates satisfied (empty input)" $
      isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "")
    it "succeeds when all predicates satisfied" $
      parse (satisfyAny (isUpper :. (/= 'X') :. Nil)) "ABc" `shouldBe` Result "Bc" 'A'
    it "succeeds when one of two predicates satisfied" $
      parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "ABc" `shouldBe` Result "Bc" 'A'

  describe "betweenSepbyCommaTest" $ do
    it "fails when opening char missing" $
      isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]")
    it "fails when closing char missing" $
      isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a")
    it "fails when input between seperators doesn't match (multiple matches)" $
      isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]")
    it "fails when input between seperators doesn't match" $
      isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]")
    it "succeeds --- one match" $
      parse (betweenSepbyComma '[' ']' lower) "[a]" `shouldBe` Result "" "a"
    it "succeeds --- nothing between surrounds" $
      parse (betweenSepbyComma '[' ']' lower) "[]" `shouldBe` Result "" ""
    it "succeeds --- 3 matches" $
      parse (betweenSepbyComma '[' ']' lower) "[a,b,c]" `shouldBe` Result "" "abc"
    it "succeeds --- 3 padded matches" $
      parse (betweenSepbyComma '[' ']' lower) "[a,  b, c]" `shouldBe` Result "" "abc"
    it "succeeds --- digits1" $
      parse (betweenSepbyComma '[' ']' digits1) "[123,456]" `shouldBe` Result "" ("123":."456":.Nil)
