import Test.DocTest
main :: IO ()
main = doctest ["-isrc", "Intcode", "Intcode.Machine", "Intcode.Opcode", "Intcode.Parse"]
