import LZ.LZ78Spec
import LZ.LZWSpec
import RLESpec
import Statistic.HuffmanSpec
import Statistic.ShannonFanoSpec
import Statistic.EncodingTreeSpec

main :: IO Bool
main = do
    _ <- LZ.LZ78Spec.runTests
    _ <- RLESpec.runTests
    _ <- LZ.LZWSpec.runTests
    _ <- Statistic.HuffmanSpec.runTests
    _ <- Statistic.ShannonFanoSpec.runTests
    Statistic.EncodingTreeSpec.runTests
  
