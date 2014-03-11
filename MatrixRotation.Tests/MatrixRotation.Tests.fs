[<NUnit.Framework.TestFixture>]
module MatrixRotation.Tests

open FsUnit
open NUnit.Framework
open MatrixRotation

let values = array2D [[1;2];[3;4]]
let flat = array2D [[1; 2; 3; 4]]
let tall = array2D [[1];[2];[3];[4]]

let fourTimes (f) (values: 'a[,]) = values |> f |> f |> f |> f

[<Test>]
let ``reverseColumn returns column values listed in reverse order`` () =
    values |> reverseColumn 0 |> should equal [3;1] 
    values |> reverseColumn 1 |> should equal [4;2]

[<Test>]
let ``rotateClockwise should rotate clockwise`` () =
    values |> rotateClockwise |> should equal <| array2D [[3;1];[4;2]]

[<Test>]
let ``rotateCounterClockwise should rotate counterclockwise`` () =
    values |> rotateCounterClockwise |> should equal <| array2D [[2;4];[1;3]]

[<Test>]
let ``rotateClockwise 4 times should produce initial value `` () =
    values |> fourTimes rotateClockwise |> should equal values

[<Test>]
let ``rotateCounterClockwise 4 times should produce initial value `` () =
    values |> fourTimes rotateCounterClockwise |> should equal values

[<Test>] 
let ``transform flat into tall`` ()  = 
    flat |> rotateClockwise |> should equal tall

[<Test>] 
let ``transform tall into flat`` ()  = 
    tall |> rotateCounterClockwise |> should equal flat

