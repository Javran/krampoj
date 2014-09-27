import Language.Scheme.Krampoj.Parser

import Text.Parsec

idTestStr :: String
idTestStr = "lambda q list->vector soup \
            \+ V17a <=? a34kTMNs \
            \the-word-recursion-has-many-meanings"

main :: IO ()
main = putStrLn idTestStr
    >> print result
    where
        result = parse (identifier `sepBy1` space) "" idTestStr
