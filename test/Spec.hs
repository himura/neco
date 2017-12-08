import Test.Tasty

tests :: TestTree
tests = testGroup "hoge" []

main :: IO ()
main = defaultMain tests
