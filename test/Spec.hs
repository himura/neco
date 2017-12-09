import qualified Filter.Response
import Test.Tasty

tests :: TestTree
tests = testGroup "rio-test" [Filter.Response.tests]

main :: IO ()
main = defaultMain tests
