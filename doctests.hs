import Test.DocTest
main :: IO ()
main = doctest ["--verbose", "-isrc", "Intcode", "Intcode.Machine", "Intcode.Opcode", "Intcode.Parse"]
