module WildCard (wildcard) where

import Data.List

wildcard :: String -> String -> Maybe Error
wildcard consumer input = go False input $ split '*' consumer where
    -- End of input
    go wildc ""    cons | null cons                 = Nothing
                        | wildc && all (=="*") cons = Nothing
                        | otherwise                 = unexpected ""
    -- End of consumers
    go True input  []       = Nothing
    go False input []       = unexpected input
    -- Set wild card to `on'
    go wildc input ("*":xs) = go True input xs
    -- Try to consume immediately
    go False input (x  :xs) = case consume x input of
                                Just new -> go False new xs
                                Nothing  -> unexpected input
    -- Wildcard is on; try to consume until the end of the string
    go True input  (x  :xs) = case walkConsume x input of
                                Just new -> go False new xs
                                Nothing  -> unexpected input

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

-}
