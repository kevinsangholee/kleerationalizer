module Test.Generated.Main3870951823 exposing (main)

import RTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "RTest" [RTest.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 180391615110502, processes = 12, paths = ["/mnt/d/Pomona/19-20/181n/Project/tests/RTest.elm"]}