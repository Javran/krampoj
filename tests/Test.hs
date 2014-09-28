import Test.HUnit

import Language.Scheme.Krampoj.Parser
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative
import System.Exit
import Control.Monad

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

        shouldSucceed = map ((~=?) <$> Just <*> (parseAllMaybe identifier))
                            validIdentifiers
        shouldFail = map (\x -> Nothing ~=? parseAllMaybe identifier x)
                         (words "1234 1234x ;;aaaAAA")

main :: IO ()
main = runTestFailIm testIdentifier
