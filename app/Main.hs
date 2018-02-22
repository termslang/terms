module Main where


import System.Environment

import Emasm

main :: IO ()
main = do
  a <- getArgs
  b <- readFile . head $ a
  let c = unfoldPseudoasm . parseAsmFileContents $ b
  let err = testErrors c
  if length err /= 0 then mapM_ print err else do
    let d = computeBytecode c
    mapM_ print d

    let e = finalizeBytecode d
    print e




{-
main = getArgs
  >>= return . head
  >>= readFile
  >>= return . parseAsmFileContents
  >>= return . unfoldPseudoasm
  >>= return . finalizeBytecode
  -- >>= return . computeBytecode

  -- >>= return . uniteBytecode
  -- >>= return . addLoader
  >>= print
  -- >>= mapM_ print
-}
