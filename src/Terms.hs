
module Terms where

{-
import System.Environment
import System.IO
import Control.Monad
import Data.List
import Data.Char


import Text.Regex



-- readTrack :: String -> IO Track
-- readTrack file =
--   do defFile <- readFile file
--      let fileLines = lines defFile





wordsSplit :: String -> [String]
wordsSplit s
  | findSpace == [] = []
  | otherwise = w : wordsSplit ws
  where
    (w, ws) = break notChar findSpace
    findSpace = dropWhile notChar s
    notChar c = elem c ".;: \t\n\r\f\v"


sentenceSplit :: String -> [String]
sentenceSplit s
  | findSpace == [] = []
  | otherwise = w : sentenceSplit ws
  where
    (w, ws) = break notChar findSpace
    findSpace = dropWhile notChar s
    notChar c = elem c ".;:"





--
-- isSpace' c  = [c == ' '  || c == '\t' || c == '\n' ||
--              c == '\r' || c == '\f' || c == '\v']



-- sectionBegins :: [String] -> Bool
-- sectionBegins a
--   | True = t == "CONTRACT" || t == "CONDITIONS"
--   | False = otherwise
--   where
--     t = map toUpper (head a)


initSectionBegins a
  | True = map toUpper (head a) /= "CONTRACT"
  | False = otherwise
mainSectionBegins a
  | True = map toUpper (head a) /= "CONDITIONS"
  | False = otherwise




-- sectionBegins :: [String] -> Bool
-- sectionBegins a
--   | True = t == "CONTRACT" || t == "CONDITIONS"
--   | False = otherwise
--   where
--     t = map toUpper (takeWhile isWord a)

    -- isWord c = [c == ' '  || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v']


-- splitByWord :: String -> [String]
-- splitByWord = splitRegex (mkRegex "[^a-zA-Z]+")


main = do
          args <- getArgs
          let filename = (head args)
          contents <- readFile filename


          let sections = map unwords $ dropWhile initSectionBegins .
                         map words . filter (not . null) $ (lines contents)

-- split by methods

          -- let methods = separateBy chr = unfoldr sep
          --   where
          --     sep [] = Nothing
          --     sep l  = Just . fmap (drop 1) . break (== chr) $ l
          --    -- splitOn () isMethod

          let a = sentenceSplit . unwords $ sections

          mapM_ print a

          return a

          let out = unlines a
          let newContents = map toUpper contents
          when (length newContents > 0) $
            writeFile "a.txt" out


            --TODO:

            count variables
            detect   CONSTANT transferFrom(address from, address to, uint256 value)
            detect   PAYABLE transferFrom(address from, address to, uint256 value)
                     EVENT Approval(address indexed _owner, address indexed _spender, uint256 _value);
            split on methods
-}
