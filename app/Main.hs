module Main where


import System.Environment

import Emasm

main :: IO ()
main = (getArgs)
  >>= return . head
  >>= readFile
  >>= return . parseAsmFileContents
  -- >>= return . unfoldPseudoasm
  -- >>= return . splitInitSection
  >>= return . computeBytecode
  -- >>= return . uniteBytecode
  -- >>= return . addLoader
  -- >>= print

  >>= mapM_ print

  -- >>= mapM_ printASMLine
