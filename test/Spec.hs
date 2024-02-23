import LZ78Spec
import RLESpec

main :: IO Bool
main = do
    LZ78Spec.runTests
    RLESpec.runTests
  
