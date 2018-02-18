module Common
  ( keccakHash
  , showHex
  , hexFromInstr
  ) where


import Data.Char
import Numeric (showIntAtBase)

import Data.Text.Encoding   (encodeUtf8)
import Data.Text.Conversions
import Crypto.Hash          (hash, Keccak_256, Digest)
keccakHash :: String -> String
keccakHash x = show (hash (encodeUtf8 (convertText (x))) :: Digest Keccak_256)


showHex :: (Integral a, Show a) => a -> String
showHex x = if odd (length s) then '0':s else s
  where s = (showIntAtBase 16 intToDigit) x ""


hexFromInstr :: String -> String -> String
hexFromInstr instr operand =
  case instr of
    "PUSH" -> showHex (0x5f + (length operand) `div` 2 - 1) ++ drop 2 operand
    _ -> yy !! 0
  where
    yy = [ y | (x,y) <- l , x == instr ] ++ [""]
    l = [
      ("STOP", "00"),
      ("ADD", "01"),
      ("MUL", "02"),
      ("SUB", "03"),
      ("DIV", "04"),
      ("SDIV", "05"),
      ("MOD", "06"),
      ("SMOD", "07"),
      ("ADDMOD", "08"),
      ("MULMOD", "09"),
      ("EXP", "0a"),
      ("SIGNEXTEND", "0b"),

      ("LT", "10"),
      ("GT", "11"),
      ("SLT", "12"),
      ("SGT", "13"),
      ("EQ", "14"),
      ("ISZERO", "15"),
      ("AND", "16"),
      ("OR", "17"),
      ("XOR", "18"),
      ("NOT", "19"),
      ("BYTE", "1a"),

      ("SHA3", "20"),

      ("ADDRESS", "30"),
      ("BALANCE", "31"),
      ("ORIGIN", "32"),
      ("CALLER", "33"),
      ("CALLVALUE", "34"),
      ("CALLDATALOAD", "35"),
      ("CALLDATASIZE", "36"),
      ("CALLDATACOPY", "37"),
      ("CODESIZE", "38"),
      ("CODECOPY", "39"),
      ("GASPRICE", "3a"),
      ("EXTCODESIZE", "3b"),
      ("EXTCODECOPY", "3c"),
      ("RETURNDATASIZE", "3d"),
      ("RETURNDATACOPY", "3e"),

      ("BLOCKHASH", "40"),
      ("COINBASE", "41"),
      ("TIMESTAMP", "42"),
      ("NUMBER", "43"),
      ("DIFFICULTY", "44"),
      ("GASLIMIT", "45"),

      ("POP", "50"),
      ("MLOAD", "51"),
      ("MSTORE", "52"),
      ("MSTORE8", "53"),
      ("SLOAD", "54"),
      ("SSTORE", "55"),
      ("JUMP", "56"),
      ("JUMPI", "57"),
      ("PC", "58"),
      ("MSIZE", "59"),
      ("GAS", "5a"),
      ("JUMPDEST", "5b"),

      ("DUP1", "80"),
      ("DUP2", "81"),
      ("DUP3", "82"),
      ("DUP4", "83"),
      ("DUP5", "84"),
      ("DUP6", "85"),
      ("DUP7", "86"),
      ("DUP8", "87"),
      ("DUP9", "88"),
      ("DUP10", "89"),
      ("DUP11", "8a"),
      ("DUP12", "8b"),
      ("DUP13", "8c"),
      ("DUP14", "8d"),
      ("DUP15", "8e"),
      ("DUP16", "8f"),

      ("SWAP1", "90"),
      ("SWAP2", "91"),
      ("SWAP3", "92"),
      ("SWAP4", "93"),
      ("SWAP5", "94"),
      ("SWAP6", "95"),
      ("SWAP7", "96"),
      ("SWAP8", "97"),
      ("SWAP9", "98"),
      ("SWAP10", "99"),
      ("SWAP11", "9a"),
      ("SWAP12", "9b"),
      ("SWAP13", "9c"),
      ("SWAP14", "9d"),
      ("SWAP15", "9e"),
      ("SWAP16", "9f"),

      ("LOG0", "a0"),
      ("LOG1", "a1"),
      ("LOG2", "a2"),
      ("LOG3", "a3"),
      ("LOG4", "a4"),
-- opcodes е1, е2, е3 deprecated
      ("CREATE", "f0"),
      ("CALL", "f1"),
      ("CALLCODE", "f2"),
      ("RETURN", "f3"),
      ("DELEGATECALL", "f4"),
      ("CALLBLACKBOX", "f5"),

      ("STATICCALL", "fa"),
      ("REVERT", "fd"),
      ("INVALID", "fe"),

      ("SELFDESTRUCT", "ff"),
      ("SUICIDE", "ff")
      ]
