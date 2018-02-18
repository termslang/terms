module Emasm
  ( parseAsmFileContents
  , unfoldPseudoasm
  , splitInitSection
  , computeBytecode
  , extractMethods
  , uniteBytecode
  , addLoader
  -- , printASMLine
  ) where


import Data.List
import Data.Char
import Common (keccakHash, showHex, hexFromInstr)



data ASMLine = ASMLine  -- String String
  { instr    :: String
  , operand  :: String
  , bytecode :: String
  , offset   :: Int
  }
  deriving (Eq)
instance Show ASMLine where
  show (ASMLine instr operand bytecode offset) = "(" ++ init (show instr) ++ " " ++ tail (show operand) ++ ",          " ++ show bytecode ++ ", offset " ++ show offset ++ ")"


parseAsmFileContents :: String -> [ASMLine]
parseAsmFileContents =
  map (\(x,y) -> ASMLine {instr = x, operand = dropWhile isSpace y, bytecode = "", offset = 0}) .
  map (break isSpace) . filter (not . null) . map (dropWhileEnd isSpace) .
  map (dropWhile isSpace) . map (takeWhile (/=';')) . lines




-- unfoldPush :: (String,String) -> (String,String)
-- unfoldPush (x,y)
--   | (x == "PUSH") = ("PUSH" ++ (show n), y)
--   | otherwise     = (x,y)
--   where
--     n = (length y) `div` 2 - 1


splitInitSection :: [(String,String)] -> ([(String,String)], [(String,String)])
splitInitSection arr = (\(x,y) -> (x, drop 1 y)) $ break (==("INIT","")) arr


isMethodSignature :: String -> Bool
isMethodSignature x
  | elem '(' x && last x == ')'    = True
  | otherwise                      = False


--TODO: index methods in list
extractMethods :: [(String,String)] -> [String]
extractMethods asm = [ y | (x,y) <- asm, ((x == "JUMPDEST" || x == "FALLBACK") && isMethodSignature y) ]

extractTags :: [(String,String)] -> [String]
extractTags asm = [ y | (x,y) <- asm, ((x == "JUMPDEST" || x == "FALLBACK")) ]



-- unfoldFallback :: (String,String) -> (String,String)
-- unfoldFallback (x,y)




unfoldPseudoasm :: [ASMLine] -> [ASMLine]
unfoldPseudoasm pseudoasm = pseudoasm





precomputeBytecode :: [ASMLine] -> [ASMLine]
precomputeBytecode asm = map (\x -> x {bytecode = hexFromInstr (instr x)(operand x)}) asm


computeBytecode :: [ASMLine] -> [ASMLine]
-- computeBytecode asm = precomputeBytecode asm
computeBytecode asm = resolveJumps $ precomputeBytecode asm



uniteBytecode :: [ASMLine] -> String
uniteBytecode asm = foldl (++) "" [ bytecode x | x <- asm ]
-- uniteBytecode :: [(String,String,String)] -> String
-- uniteBytecode asm = foldl (++) "" [ z | (_,_,z) <- asm , True ]


-- totalLength :: [ASMLine] -> Int
-- totalLength a = foldl (+) 0 [ length (bytecode x) | x <- a ]

computeOffsets :: [ASMLine] -> [ASMLine]
computeOffsets [] = []
computeOffsets (a:as) = let
  t = length (uniteBytecode (a:as))
  v = foldl (+) 0 [ length (bytecode x) | x <- as ]

  in a {offset = v}: computeOffsets as
  -- where
  -- oo = foldl (+) 0 (offset (a:as))
  -- oo = scanl (+) 0 . map (\x -> length (bytecode x)) $ (a:as)

{-
computeOffsets :: [ASMLine] -> [ASMLine]
computeOffsets :: [a] -> [b] -> [(a,b)]
computeOffsets []     _bs    = []
computeOffsets _as    []     = []
computeOffsets (a:as) (b:bs) = (a,b) : computeOffsets as bs
  where
  oo = scanl (+) 0 . map (\x -> length (bytecode x)) $ asm
-}


--
-- computeOffsets :: [ASMLine] -> [ASMLine]
-- -- computeOffsets asm = map (\x -> x {offset = oo}) asm
-- computeOffsets asm =
--   -- [ x {offset = y} | x <- asm , y <- oo ]
--   map (\x -> x {offset = 2}) asm
--
--   -- map (\x -> x {offset = oo}) asm
--   where
--   oo = scanl (+) 0 . map (\x -> length (bytecode x)) $ asm
-- --  foldl (+) (length (bytecode x) `div` 2)--  (\x -> x {offset = length (bytecode x) `div` 2}) asm
-- -- computeOffsets asm = map (\x -> x {offset = length (bytecode x) `div` 2}) asm

  -- l = map (\x -> length (bytecode x)) $ asm
  -- l = head . scanl (+) 0 . map (\x -> length (bytecode x)) $ asm
  -- -- in foldr (\(x,y,z) -> (x,y,z,show l)) asm ("","","","")
  -- in map (\(x,y,z) -> (x,y,z,show l)) asm


resolveJumps :: [ASMLine] -> [ASMLine]
resolveJumps asm = computeOffsets asm










addLoader :: String -> String
addLoader bytecode = let
  initial = "hdfhsbhb"
  codesize = length bytecode `div` 2
  codepush = if codesize > 0xff then "61" else "60"
  a = initial ++ codepush ++ showHex codesize ++ "80"
  codeoffset1 = length a `div` 2 + 8
  initpush = if codeoffset > 0xff then "61" else "60"
  codeoffset = if codeoffset1 > 0xff then codeoffset1 + 1 else codeoffset1
  b = a ++ initpush ++ showHex codeoffset ++ "6000396000f3" ++ bytecode
  in b
