import Data.Char
import Text.Printf

{-
WIDTH=12;
DEPTH=4096;
ADDRESS_RADIX=HEX;
DATA_RADIX=HEX;
CONTENT
-}

readHexCode :: FilePath -> IO [String]
readHexCode f = do
  c <- readFile f
  return $ lines $ map toUpper c

writeMif :: FilePath -> [String] -> IO ()
writeMif f c = do
  let width = "WIDTH=32;\n"
  let depth = "DEPTH=" ++ (show $ length c) ++ ";\n"
  let ar = "ADDRESS_RADIX=HEX;\n"
  let dr = "DATA_RADIX=HEX;\n"
  let content = ""
  let mifData = concat $ zipWith (\a b -> a ++ " : " ++ b ++ "\n") (map (printf "%03X" :: Int -> String) [0..]) c
  -- writeFile f (width ++ depth ++ ar ++ dr ++)
  return ()

main :: IO ()
main = do
  s <- readHexCode "./code/fib_h.txt"
  putStrLn $ concat $ zipWith (\a b -> a ++ " : " ++ b ++ "\n") (map (printf "%03X" :: Int -> String) [0..]) s
  return ()