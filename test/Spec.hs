import LZ.LZ78Spec
import LZ.LZWSpec
import RLESpec
import Statistic.HuffmanSpec
import Statistic.ShannonFanoSpec
import Statistic.EncodingTreeSpec

main :: IO Bool
main = do
    LZ.LZ78Spec.runTests
    RLESpec.runTests
    LZ.LZWSpec.runTests
    Statistic.HuffmanSpec.runTests
    Statistic.ShannonFanoSpec.runTests
    Statistic.EncodingTreeSpec.runTests
  
