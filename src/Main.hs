import Runtime
import Parser

go :: String -> String
go input = show $ execute $ parse input

main = interact go
