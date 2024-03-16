import LZ.LZ78Spec
import LZ.LZWSpec
import RLESpec
import Statistic.HuffmanSpec
import Statistic.ShannonFanoSpec
import Statistic.EncodingTreeSpec
import Statistic.SourceSpec

main :: IO Bool
main = do
    _ <- LZ.LZ78Spec.runTests
    _ <- RLESpec.runTests
    _ <- LZ.LZWSpec.runTests
    _ <- Statistic.HuffmanSpec.runTests
    _ <- Statistic.ShannonFanoSpec.runTests
    _ <- Statistic.SourceSpec.runTests
    Statistic.EncodingTreeSpec.runTests
  
