-- Emanuel Sjövall, Rafael Holgersson
module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]

newtype Rule = Rule (Pattern String, [Template String])
  deriving (Eq, Show)

type BotBrain = [Rule]

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()


--------------------------------------------------------

-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b =
  fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
{- TO BE WRITTEN -}
makePair (Rule (pattern, templates)) = do
  r <- randomIO :: IO Float
  return (pattern, pick r templates)

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
-- rulesApply list p = fromMaybe p (transformationsApply reflect list p)
rulesApply list = try (transformationsApply reflect list)

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = map (\x -> fromMaybe x (lookup x reflections))

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
{- TO BE WRITTEN -}
ruleCompile (toPattern, toTemplates) = Rule (starPattern (map toLower toPattern), map starPattern toTemplates)

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: Eq a => a -> [a] -> Pattern a
{- TO BE WRITTEN -}
mkPattern wc = Pattern . map (\x -> if x == wc then Wildcard else Item x)

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions = (map . map2) (starPattern, starPattern)
  [ ( "please *", "*" ),
    ( "could you *", "*" ),
    ( "can you *", "*"),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply list = fix (\x -> fromMaybe x (transformationsApply id list x))

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a template with the list given as the third argument
substitute :: Eq a => Template a -> [a] -> [a]
{- TO BE WRITTEN -}
substitute (Pattern []) _ = []
substitute (Pattern (x:xs)) sub =
  (case x of
    Item s -> [s]
    Wildcard -> sub
  ) ++ substitute (Pattern xs) sub

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => Pattern a -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
match (Pattern []) [] = Just []
match (Pattern []) _ = Nothing
match (Pattern _) [] = Nothing
match (Pattern (Item i:ps)) (x:xs) = if i /= x then Nothing else match (Pattern ps) xs
-- match (Pattern (Wildcard:ps)) xs = orElse (singleWildcardMatch (Pattern (Wildcard : ps)) xs) (longerWildcardMatch (Pattern (Wildcard : ps)) xs)
match ps xs = orElse (singleWildcardMatch ps xs) (longerWildcardMatch ps xs)

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _ -> Just [x]
{- TO BE WRITTEN -}
longerWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern (Wildcard:ps)) xs of
    Nothing -> Nothing
    Just res -> Just (x : res)

-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)

-- Applying a single pattern
transformationApply :: Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
{- TO BE WRITTEN -}
transformationApply f str (pattern, template) =  mmap (substitute template) (match pattern (f str))

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
transformationsApply _ [] _ = Nothing
transformationsApply f (pattern:ps) str =
  case transformationApply f str pattern of
    Nothing -> transformationsApply f ps str
    Just v -> Just v
