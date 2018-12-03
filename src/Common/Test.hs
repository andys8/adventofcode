module Common.Test (runTests) where

runTests f testData = sequence_ $ map (putStrLn . runTest f) testData

runTest f (given, expected) = if actual == expected
  then "Passed: " ++ show given
  else
    "Failed: "
    ++ (show actual)
    ++ " (actual) != "
    ++ (show expected)
    ++ " (expected)"
  where actual = f given
