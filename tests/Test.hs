import Test.HUnit

import Language.Scheme.Krampoj.Parser
import Text.Megaparsec
import Text.Megaparsec.String (Parser)

import System.Exit
import Control.Monad
import Data.Char

parseAllMaybe :: Parser a -> String -> Maybe a
parseAllMaybe p x = case parse (p <* eof) "" x of
                      Left _ -> Nothing
                      Right v -> Just v

runTestFailIm :: Test -> IO ()
runTestFailIm t = do
    cs <- runTestTT t
    print cs
    when (errors cs > 0 || failures cs > 0)
         exitFailure

testIdentifier :: Test
testIdentifier =
    "identifier" ~: TestList $ shouldSucceed ++ shouldFail
    where
        validIdentifiers = words "lambda q list->vector soup \
                                 \+ V17a <=? a34kTMNs \
                                 \the-word-recursion-has-many-meanings"

        shouldSucceed = map ((~=?) <$> Just <*> parseAllMaybe identifier)
                            validIdentifiers
        shouldFail = map (\x -> Nothing ~=? parseAllMaybe identifier x)
                         (words "1234 1234x ;;aaaAAA")

testCIString :: Test
testCIString =
    "ciString" ~: TestList $ map makeTestcase testStrs
    where
        testStrs = words "AAA bbb #e #E #I #i #whatever #WhAtE4321EvEr"
        makeTestcase s = Just s ~=? parseAllMaybe (ciString s') s
            where
                s' = map toLower s

main :: IO ()
main = mapM_ runTestFailIm [ testIdentifier
                           , testCIString
                           ]
