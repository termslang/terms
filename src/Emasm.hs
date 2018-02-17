module Emasm
  ( parseAsmFileContents
  , unfoldPseudoasm
  , splitInitSection
  , computeBytecode
  , extractMethods
  , uniteBytecode
  , addLoader
  ) where


import Data.List
import Data.Char
import Numeric (showIntAtBase)
import Data.Text.Encoding   (encodeUtf8)
import Data.Text.Conversions
import Crypto.Hash          (hash, Keccak_256, Digest)
import Text.Printf


showHex' :: (Integral a, Show a) => a -> String
showHex' x = if (length s) `mod` 2 == 1 then '0':s else s
  where s = (showIntAtBase 16 intToDigit) x ""

keccakHash :: String -> String
keccakHash x = show (hash (encodeUtf8 (convertText (x))) :: Digest Keccak_256)

hexFromInstr :: (String,String) -> String
hexFromInstr instr =
  case instr of
    ("PUSH", y) -> showHex' (0x5f + countPush y) ++ drop 2 y

    ("STOP"      ,_) -> "00"
    ("ADD"       ,_) -> "01"
    ("MUL"       ,_) -> "02"
    ("SUB"       ,_) -> "03"
    ("DIV"       ,_) -> "04"
    ("SDIV"      ,_) -> "05"
    ("MOD"       ,_) -> "06"
    ("SMOD"      ,_) -> "07"
    ("ADDMOD"    ,_) -> "08"
    ("MULMOD"    ,_) -> "09"
    ("EXP"       ,_) -> "0a"
    ("SIGNEXTEND",_) -> "0b"

    ("LT"        ,_) -> "10"
    ("GT"        ,_) -> "11"
    ("SLT"       ,_) -> "12"
    ("SGT"       ,_) -> "13"
    ("EQ"        ,_) -> "14"
    ("ISZERO"    ,_) -> "15"
    ("AND"       ,_) -> "16"
    ("OR"        ,_) -> "17"
    ("XOR"       ,_) -> "18"
    ("NOT"       ,_) -> "19"
    ("BYTE"      ,_) -> "1a"

    ("SHA3"      ,_) -> "20"

    ("ADDRESS"       ,_) -> "30"
    ("BALANCE"       ,_) -> "31"
    ("ORIGIN"        ,_) -> "32"
    ("CALLER"        ,_) -> "33"
    ("CALLVALUE"     ,_) -> "34"
    ("CALLDATALOAD"  ,_) -> "35"
    ("CALLDATASIZE"  ,_) -> "36"
    ("CALLDATACOPY"  ,_) -> "37"
    ("CODESIZE"      ,_) -> "38"
    ("CODECOPY"      ,_) -> "39"
    ("GASPRICE"      ,_) -> "3a"
    ("EXTCODESIZE"   ,_) -> "3b"
    ("EXTCODECOPY"   ,_) -> "3c"
    ("RETURNDATASIZE",_) -> "3d"
    ("RETURNDATACOPY",_) -> "3e"

    ("BLOCKHASH"  ,_) -> "40"
    ("COINBASE"   ,_) -> "41"
    ("TIMESTAMP"  ,_) -> "42"
    ("NUMBER"     ,_) -> "43"
    ("DIFFICULTY" ,_) -> "44"
    ("GASLIMIT"   ,_) -> "45"

    ("POP"        ,_) -> "50"
    ("MLOAD"      ,_) -> "51"
    ("MSTORE"     ,_) -> "52"
    ("MSTORE8"    ,_) -> "53"
    ("SLOAD"      ,_) -> "54"
    ("SSTORE"     ,_) -> "55"
    ("JUMP"       ,_) -> "56"
    ("JUMPI"      ,_) -> "57"
    ("PC"         ,_) -> "58"
    ("MSIZE"      ,_) -> "59"
    ("GAS"        ,_) -> "5a"
    ("JUMPDEST"   ,_) -> "5b"

    ("DUP1"       ,_) -> "80"
    ("DUP2"       ,_) -> "81"
    ("DUP3"       ,_) -> "82"
    ("DUP4"       ,_) -> "83"
    ("DUP5"       ,_) -> "84"
    ("DUP6"       ,_) -> "85"
    ("DUP7"       ,_) -> "86"
    ("DUP8"       ,_) -> "87"
    ("DUP9"       ,_) -> "88"
    ("DUP10"      ,_) -> "89"
    ("DUP11"      ,_) -> "8a"
    ("DUP12"      ,_) -> "8b"
    ("DUP13"      ,_) -> "8c"
    ("DUP14"      ,_) -> "8d"
    ("DUP15"      ,_) -> "8e"
    ("DUP16"      ,_) -> "8f"

    ("SWAP1"      ,_) -> "90"
    ("SWAP2"      ,_) -> "91"
    ("SWAP3"      ,_) -> "92"
    ("SWAP4"      ,_) -> "93"
    ("SWAP5"      ,_) -> "94"
    ("SWAP6"      ,_) -> "95"
    ("SWAP7"      ,_) -> "96"
    ("SWAP8"      ,_) -> "97"
    ("SWAP9"      ,_) -> "98"
    ("SWAP10"     ,_) -> "99"
    ("SWAP11"     ,_) -> "9a"
    ("SWAP12"     ,_) -> "9b"
    ("SWAP13"     ,_) -> "9c"
    ("SWAP14"     ,_) -> "9d"
    ("SWAP15"     ,_) -> "9e"
    ("SWAP16"     ,_) -> "9f"

    ("LOG0"       ,_) -> "a0"
    ("LOG1"       ,_) -> "a1"
    ("LOG2"       ,_) -> "a2"
    ("LOG3"       ,_) -> "a3"
    ("LOG4"       ,_) -> "a4"
-- opcodes е1, е2, е3 deprecated

    ("CREATE"        ,_) -> "f0"
    ("CALL"          ,_) -> "f1"
    ("CALLCODE"      ,_) -> "f2"
    ("RETURN"        ,_) -> "f3"
    ("DELEGATECALL"  ,_) -> "f4"
    ("CALLBLACKBOX"  ,_) -> "f5"

    ("STATICCALL"    ,_) -> "fa"
    ("REVERT"        ,_) -> "fd"
    ("INVALID"       ,_) -> "fe"

    ("SELFDESTRUCT"  ,_) -> "ff"
    ("SUICIDE"       ,_) -> "ff"

    (_,_) -> ""



data ASMLine = ASMLine
  { instr    :: String
  , operand  :: String
  , bytecode :: String
  } deriving (Show, Eq)
printASMLine :: ASMLine -> String
printASMLine r =
  printf
    "%s %s, bytecode: %s"
    (instr r)
    (operand r)
    (bytecode r)



parseAsmFileContents :: String -> [(String,String)]
parseAsmFileContents  = map (\(x,y) -> (x, dropWhile isSpace y)) .
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




unfoldPseudoasm :: [(String,String)] -> [(String,String)]
-- unfoldPseudoasm :: [(String,String)] -> [(String,String)]
unfoldPseudoasm pseudoasm = pseudoasm
-- unfoldPseudoasm pseudoasm = map (\(x,y) -> (x,keccakHash y)) pseudoasm
-- unfoldPseudoasm pseudoasm = map (\(x,y) -> (x,y ++ show (isMethodSignature y))) pseudoasm
-- unfoldPseudoasm pseudoasm = map unfoldPush pseudoasm





countPush :: String -> Int
countPush y = (length y) `div` 2 - 1


precomputeBytecode :: [(String,String)] -> [(String,String,String)]
precomputeBytecode asm = map (\(x,y) -> (x,y,hexFromInstr (x,y))) asm





computeOffsets :: [(String,String,String)] -> [(String,String,String,String)]
computeOffsets asm = let
  l = scanl (+) 0 . map (\(_,_,xs) -> length xs) $ asm
  -- in foldr (\(x,y,z) -> (x,y,z,show l)) asm ("","","","")
  in map (\(x,y,z) -> (x,y,z,show l)) asm


resolveJumps :: [(String,String,String)] -> [(String,String,String,String)]
resolveJumps asm = computeOffsets asm



computeBytecode :: [(String,String)] -> [(String,String,String,String)]
computeBytecode asm = resolveJumps $ precomputeBytecode asm


uniteBytecode :: [(String,String,String)] -> String
uniteBytecode asm = foldl (++) "" [ z | (_,_,z) <- asm , True ]





addLoader :: String -> String
addLoader bytecode = let
  initial = "hdfhsbhb"
  codesize = length bytecode `div` 2
  codepush = if codesize > 0xff then "61" else "60"
  a = initial ++ codepush ++ showHex' codesize ++ "80"
  codeoffset1 = length a `div` 2 + 8
  initpush = if codeoffset > 0xff then "61" else "60"
  codeoffset = if codeoffset1 > 0xff then codeoffset1 + 1 else codeoffset1
  b = a ++ initpush ++ showHex' codeoffset ++ "6000396000f3" ++ bytecode
  in b
