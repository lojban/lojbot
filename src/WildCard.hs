module WildCard (wildcard) where

import Data.List

wildcard :: String -> String -> Maybe Error
wildcard consumer input = go False input $ split '*' consumer where
    -- End of input
    go wildc ""    cons | null cons                   = Nothing
                        | wildc && all (=="*") cons   = Nothing
                        | not wildc && cons == ["*"]  = Nothing
                        | otherwise                   = unexpected "end of input"
    -- End of consumers
    go True  input []        = Nothing
    go False input []        = unexpected input
    -- Set wild card to `on'
    go wildc input ("*":xs)  = go True input xs
    -- Try to consume immediately
    go False input (x  :xs)  = tryConsume xs $ consume x input
    -- Wildcard is on; try to consume until the end of the string
    go True  input (x  :xs)  = tryConsume xs $ walkConsume x input
    -- Try to consume from the input
    tryConsume xs = maybe (unexpected input) (flip (go False) xs)

unexpected :: String -> Maybe Error
unexpected e = Just $ "unexpected \"" ++ e ++ "\""

split :: Char -> String -> [String]
split c = filter (/="") . go where
    go [] = []
    go xs = let (ys,xs') = break (==c) xs
            in ys : take 1 xs' : go (drop 1 xs')

consume :: String -> String -> Maybe String
consume cons inp | isPrefixOf cons inp = Just $ drop (length cons) inp
                 | otherwise           = Nothing

walkConsume :: String -> String -> Maybe String
walkConsume cons = fmap (drop (length cons)) . findLast (isPrefixOf cons) . tails

-- Equivalent to: findLast p = find p . reverse
findLast :: (a -> Bool) -> [a] -> Maybe a
findLast p = foldl try Nothing where
    try last x | p x       = Just x
               | otherwise = last

type Error = String

{-

*WildCard> wildcard "hello" ""
Just "unexpected \"\""
*WildCard> wildcard "hello" "hello"
Nothing
*WildCard> wildcard "hello" "hello dave"
Just "unexpected \" dave\""
*WildCard> wildcard "hello*" "hello dave"
Nothing
*WildCard> wildcard "hello*" "oh, hello dave"
Just "unexpected \"oh, hello dave\""
*WildCard> wildcard "*hello*" "oh, hello dave"
Nothing
*WildCard> wildcard "*hello*" "oh, hel!!lo dave"
Just "unexpected \"oh, hel!!lo dave\""
*WildCard> wildcard "*hel*lo*" "oh, hel!!lo dave"
Nothing
*WildCard> wildcard "hello" ""
Just "unexpected \"end of input\""
*WildCard> wildcard "hello*" ""
Just "unexpected \"end of input\""
*WildCard> wildcard "hello*" "hello"
Nothing
*WildCard> wildcard "*hello*" "hello"
Nothing
*WildCard> wildcard "*hello*" "fhello"
Nothing
*WildCard> wildcard "*hello*" "fhellof"
Nothing
*WildCard> wildcard "*hello*k" "fhellof"
Just "unexpected \"f\""
*WildCard> wildcard "*hello*k" "fhellofk"
Nothing
*WildCard> wildcard "*hello*k*" "fhellofk"
Nothing

-}
