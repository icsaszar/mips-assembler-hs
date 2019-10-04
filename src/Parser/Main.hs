module Parser.Main where
import Parser.Parser (stringParser, fileParser)
import Parser.Grammar (Program(..))


main = do
    fileName <- getLine
    result <- fileParser fileName
    case result of Lines lns -> mapM_ print lns
    
