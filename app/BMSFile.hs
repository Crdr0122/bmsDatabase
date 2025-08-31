module BMSFile where

import Schema (BMSFile)

processBMS file = do
  result <- readFile file
  return ()
