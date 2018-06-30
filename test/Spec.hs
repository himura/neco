import qualified Filter.Request
import qualified Filter.Response
import qualified Filter.RetryFilter
import Test.Tasty

tests :: TestTree
tests = testGroup "rio-test" [Filter.Request.tests, Filter.Response.tests, Filter.RetryFilter.tests]

main :: IO ()
main = defaultMain tests
