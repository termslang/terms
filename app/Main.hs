module Main where


import System.Environment
import System.IO

import Emasm


main = (getArgs)
  >>= return . head
  >>= readFile
  >>= return . parseAsmFileContents
  >>= return . unfoldPseudoasm
  >>= return . extractMethods

--  >>= return . compileToBytecode
  >>= mapM_ print
