module Emasm
  ( parseAsmFileContents
  , unfoldPseudoasm
  , splitInitSection
  , computeBytecode
  , extractMethods
  , addLoader
  -- , printASMLine
  ) where


import Data.List
import Data.Char
import Common (keccakHash, showHex, hexFromInstr)


data ASMLine = ASMLine
  { instr    :: String
  , operand  :: String
  , bytecode :: String
  , offset   :: String
  }
  deriving (Eq)
instance Show ASMLine where
  show (ASMLine instr operand bytecode offset) = "(" ++ init (show instr) ++ " " ++ tail (show operand) ++ ",          " ++ show bytecode ++ if instr == "JUMPDEST" then ", offset " ++ operand ++ ":" ++ show offset else "" ++ ")"


parseAsmFileContents :: String -> [ASMLine]
parseAsmFileContents =
  map (\(x,y) -> ASMLine {instr = x, operand = dropWhile isSpace y, bytecode = "", offset = ""}) .
  map (break isSpace) . filter (not . null) . map (dropWhileEnd isSpace) .
  map (dropWhile isSpace) . map (takeWhile (/=';')) . lines


offsets :: [ASMLine] -> [ASMLine]
backOffsets [] = []
backOffsets (a:as) = a {offset = showHex (length (concatMap bytecode (a:as)) `div` 2)}: backOffsets as
offsets = reverse . backOffsets . reverse





-- extractTags :: [ASMLine] -> [(String,String)]
-- extractTags asm = [ y | (x,y) <- asm, ((x == "JUMPDEST")) ]
--

precomputeBytecode :: [ASMLine] -> [ASMLine]
precomputeBytecode asm = map (\x -> x {bytecode = hexFromInstr (instr x)(operand x)}) asm


{-
findLabel :: [(String,String)] -> ASMLine -> String
findLabel labels x = let
  a = lookup (operand x) labels
  in case a of
    Just a -> a
    otherwise -> "??"


resolveJumps :: [ASMLine] -> [ASMLine]
resolveJumps asm = let
  labels = [ (operand x, offset x) | x <- asm, instr x == "JUMPDEST" ]
  bc x = case instr x of
    "JUMP" -> "56" ++ findLabel labels x
    "JUMPI" -> "57" ++ findLabel labels x
    _ -> bytecode x
  in map (\x -> x { bytecode = bc x }) asm
-}


findLabel :: [(String,String)] -> String -> String
findLabel labels x = let
  a = lookup (x) labels
  in case a of
    Just a -> a
    otherwise -> "??"


resolveJumps :: [ASMLine] -> [ASMLine]
resolveJumps asm = let
  labels = [ (operand x, offset x) | x <- asm, instr x == "JUMPDEST" ]
  bc x = case instr x of
    "JUMP" -> "56" ++ findLabel labels (operand x)
    "JUMPI" -> "57" ++ findLabel labels (operand x)
    _ -> bytecode x
--TODO: change offsets according to changes
  in map (\x -> x { bytecode = bc x }) asm



computeBytecode :: [ASMLine] -> [ASMLine]
computeBytecode = resolveJumps . offsets . precomputeBytecode





splitInitSection :: [(String,String)] -> ([(String,String)], [(String,String)])
splitInitSection arr = (\(x,y) -> (x, drop 1 y)) $ break (==("INIT","")) arr


isMethodSignature :: String -> Bool
isMethodSignature x
  | elem '(' x && last x == ')'    = True
  | otherwise                      = False


--TODO: index methods in list
extractMethods :: [(String,String)] -> [String]
extractMethods asm = [ y | (x,y) <- asm, ((x == "JUMPDEST" || x == "FALLBACK") && isMethodSignature y) ]



-- unfoldFallback :: (String,String) -> (String,String)
-- unfoldFallback (x,y)




unfoldPseudoasm :: [ASMLine] -> [ASMLine]
unfoldPseudoasm pseudoasm = pseudoasm

















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
