import qualified Filter.Request
import qualified Filter.Response
import Test.Tasty

tests :: TestTree
tests = testGroup "rio-test" [Filter.Request.tests, Filter.Response.tests]

main :: IO ()
main = defaultMain tests
