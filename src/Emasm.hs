module Emasm
  ( parseAsmFileContents
  , testErrors
  , computeBytecode
  , finalizeBytecode
  , unfoldPseudoasm
  ) where
import Data.List
import Data.Char
import Common (keccakHash, showHex, prependPush, hexFromInstr)


data ASMLine = ASMLine
  { instr    :: String
  , operand  :: String
  , bytecode :: String
  , offset   :: String
  }
  deriving (Eq)
instance Show ASMLine where
  show (ASMLine instr operand bytecode offset) = "(" ++ init (show instr) ++ " " ++ tail (show operand) ++ "," ++ show bytecode ++ if instr == "JUMPDEST" then ",offset " ++ operand ++ ":" ++ show offset ++ ")" else ")"



newASMLine :: String -> ASMLine
newASMLine = (\(x,y) -> ASMLine {instr = x, operand = dropWhile isSpace y, bytecode = "", offset = ""}) . break isSpace

parseAsmFileContents :: String -> [ASMLine]
parseAsmFileContents =
  map newASMLine . filter (not . null) . map (dropWhileEnd isSpace) .
  map (dropWhile isSpace) . map (takeWhile (/=';')) . lines


--TODO test PUSH if hex or tag
testErrors :: [ASMLine] -> [String]
testErrors (x:xs) = let
  p1 = takeWhile (\x -> instr x /= "INIT") $ (x:xs)
  p2 = drop 1 . dropWhile (\x -> instr x /= "INIT") $ (x:xs)
  notJump x p = notElem (operand x) [ operand x | x <- p, instr x == "JUMPDEST" ] && operand x /= ""
  a1 = [ instr x ++ " " ++ operand x ++ " - Error: tag not found" | x <- p1, instr x == "JUMP" || instr x == "JUMPI", notJump x p1 ]
  a2 = [ instr x ++ " " ++ operand x ++ " - Error: tag not found" | x <- p2, instr x == "JUMP" || instr x == "JUMPI", notJump x p2 ]
  b = [ instr x ++ " " ++ operand x ++ " - Error: unknown instruction" | x <- p1 ++ p2, hexFromInstr (instr x)(operand x) == "" ] :: [String]
  in a1 ++ a2 ++ b


getTagPush :: [(String,String)] -> String -> String
getTagPush tags x = let
  a = lookup (x) tags
  in case a of
    Just a -> prependPush a
    otherwise -> ""

resolveJumps :: [ASMLine] -> [ASMLine]
resolveJumps asm = let
  tags = [ (operand x, offset x) | x <- asm, instr x == "JUMPDEST" ]
  isHex x
    | length x < 2 = False
    | x !! 0 == '0' && x !! 1 == 'x' = True
    | otherwise = False
  bc x = case instr x of
    "JUMP"  -> getTagPush tags (operand x) ++ "56"
    "JUMPI" -> getTagPush tags (operand x) ++ "57"
    "PUSH"  -> if isHex (operand x) then hexFromInstr (instr x)(operand x) else getTagPush tags (operand x)
    _ -> hexFromInstr (instr x)(operand x)
  in map (\x -> x { bytecode = bc x }) asm

testJumps :: [ASMLine] -> Bool
testJumps asm = let
  a = [ offset x | x <- asm, instr x == "JUMPDEST" ] :: [String]
  offsetCorrect x = notElem (drop 2 (take (length (bytecode x) - 2) (bytecode x))) a && operand x /= ""
  b = [ x | x <- asm, instr x == "JUMP" || instr x == "JUMPI", offsetCorrect x ]
  in length b == 0

computeBytecode :: [ASMLine] -> [ASMLine]
backOffsets [] = []
backOffsets (a:as) = a {offset = showHex (length (concatMap bytecode as) `div` 2)}: backOffsets as
computeBytecode = until testJumps computeBytecode . reverse . backOffsets . reverse . resolveJumps
-- computeBytecode asm  = if testJumps asm then asm else computeBytecode . reverse . backOffsets . reverse . resolveJumps $ asm


addLoader :: String -> String -> String
addLoader initial bytecode = let
  codesize = length bytecode `div` 2
  codepush = if codesize > 0xff then "61" else "60"
  a = initial ++ codepush ++ showHex codesize ++ "80"
  codeoffset1 = length a `div` 2 + 8
  initpush = if codeoffset > 0xff then "61" else "60"
  codeoffset = if codeoffset1 > 0xff then codeoffset1 + 1 else codeoffset1
  b = a ++ initpush ++ showHex codeoffset ++ "6000396000f3" ++ bytecode
  in b

finalizeBytecode :: [ASMLine] -> String
finalizeBytecode (x:xs) = let
  p1 = concatMap bytecode . computeBytecode . takeWhile (\x -> instr x /= "INIT") $ (x:xs)
  p2 = concatMap bytecode . computeBytecode . drop 1 . dropWhile (\x -> instr x /= "INIT") $ (x:xs)
  loader = "0x" ++ addLoader p1 p2
  in loader




dispatcherLine :: String -> [ASMLine]
dispatcherLine s =
  [ newASMLine "DUP1"
  , newASMLine ("PUSH 0x" ++ (take 8 . keccakHash $ s))
  , newASMLine "EQ"
  , newASMLine ("JUMPI " ++ s)
  ]


unFALLBACK :: [ASMLine] -> [ASMLine]
isMethodSignature x
  | elem '(' x && x !! 1 /= '(' && last x == ')'    = True
  | otherwise                                       = False
methods asm = [ operand x | x <- asm, instr x == "JUMPDEST", isMethodSignature (operand x) ]
unFALLBACK (x:xs) = let
  a = takeWhile (\x -> instr x /= "FALLBACK") $ (x:xs)
  b = [ newASMLine "PUSH 0xe0"
      , newASMLine "PUSH 0x02"
      , newASMLine "EXP"
      , newASMLine "PUSH 0x00"
      , newASMLine "CALLDATALOAD"
      , newASMLine "DIV"
      ]
  -- dup = [ ASMLine {instr = "DUP1", operand = "", bytecode = "", offset = ""}]
  c = concat (map dispatcherLine (init (methods (x:xs))))
  d = drop 1 (dispatcherLine (last (methods (x:xs))))
  e = dropWhile (\x -> instr x /= "FALLBACK") $ (x:xs)
  f = (head e) {instr = "JUMPDEST"} : tail e
  in a ++ b ++ c ++ d ++ f


-- TODO remove last JUMPDEST if next is JUMPDEST, replace all last JUMPDEST operands with the next
-- TODO REFJUMPI ??
unREFJUMP :: Int -> [ASMLine] -> [ASMLine]
unREFJUMP n [] = []
unREFJUMP n (x:xs) = case instr x of
  "BACKJUMP" -> newASMLine "PUSH 0x00"
              : newASMLine "MLOAD"
              : x { instr = "JUMP", operand = "" }
              : unREFJUMP (n+1) xs
  "REFJUMP"  -> newASMLine ("PUSH REFJUMP_POINT_" ++ show n)  -- "PUSH 0x00000001"
              : newASMLine "PUSH 0x00"
              : newASMLine "MSTORE"
              : x { instr = "JUMP" }
              : newASMLine ("JUMPDEST REFJUMP_POINT_" ++ show n)
              : unREFJUMP (n+1) xs
  _ -> x: unREFJUMP (n+1) xs


unfoldPseudoasm :: [ASMLine] -> [ASMLine]
unfoldPseudoasm asm = unFALLBACK . unREFJUMP 0 $ asm
