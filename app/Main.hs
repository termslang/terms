module Main where


import System.Environment

import Emasm
-- import TestWeb3


import Network.Ethereum.Web3

import Data.Text (unpack)
import Text.Printf

-- [abiFrom|app/ERC20.json|]
-- putStr [abiFrom|data/sample.json|]

main :: IO ()
main = do
  print "a"
  s <- runWeb3 clientVersion
  print s
 -- x clientVersion
  -- putStr [abiFrom|ERC20.json|]
-- --      print (runWeb3 clientVersion)
--     Right s <- runWeb3 $ do
--         n <- name token
--         s <- symbol token
--         d <- decimals token
--         return $ printf "Token %s with symbol %s and decimals %d"
--                         (unpack n) (unpack s) d
--     putStrLn s
--   where token = "0x237D60A8b41aFD2a335305ed458B609D7667D789"

{-
main :: IO ()
main = do
  a <- getArgs
  b <- readFile . head $ a
  let c = parseAsmFileContents $ b
--  print c


  let tmp = swapYESNO c
  mapM_ print tmp

  let c1 = unfoldPseudoasm c

--  mapM_ print c1
  let err = testErrors c1
  if length err /= 0 then mapM_ print err else do
    let d = computeBytecode c1
    mapM_ print d

    let e = finalizeBytecode d
    print e

-}

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
