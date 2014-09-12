import SLCT.Constants

main :: IO()
main = do
    let x = maxLineLength constants
    putStr $ show x
