import Data.Char
import Text.Printf

readHexCode :: FilePath -> IO [String]
readHexCode f = do
  c <- readFile f
  return $ lines $ map toUpper c

writeMif :: FilePath -> [String] -> IO ()
writeMif f c = do
  let width = "WIDTH=32;\n"
  let depth = "DEPTH=" ++ show (length c) ++ ";\n"
  let ar = "ADDRESS_RADIX=HEX;\n"
  let dr = "DATA_RADIX=HEX;\n"
  let mifData = concat $ zipWith toLine (map toIndex [0 ..]) c
  let content = "CONTENT\nBEGIN\n" ++ mifData ++ "END;\n"
  writeFile f (width ++ depth ++ ar ++ dr ++ content)
  where
    toLine a b = a ++ " : " ++ b ++ "\n"

    toIndex :: Int -> String
    toIndex = printf "%03X"

main :: IO ()
main =
  readHexCode "./code/fib_h.txt"
    >>= writeMif "./code/fib.mif"