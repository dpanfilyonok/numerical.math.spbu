namespace Tests

open NUnit.Framework
open FsUnit
open NumericalMethods.NumericalIntegration
open System
open Utils

[<TestFixture>]
type TestClass () =

    let constant = fun (x: float) -> 1.
    let linear = fun (x: float) -> 2. * x
    let quadratic = fun (x: float) -> 3. * x ** 2.
    let cubic = fun (x: float) -> 4. * x ** 3.
    
    let segment = {Left = 0.; Right = 1.}

    let m = 2

    [<Test>]
    member this.``Algebraic accuracy of left rectangle rule should be 0`` () =
        let task = IntegrationTask(constant, segment)
        task.SolveWith <| LeftRectangleRule m 
        |> should (equalWithin 1e-10) 1

    [<Test>]
    member this.``Algebraic accuracy of right rectangle rule should be 0`` () =
        let task = IntegrationTask(constant, segment)
        task.SolveWith <| RightRectangleRule m 
        |> should (equalWithin 1e-10) 1

    [<Test>]
    member this.``Algebraic accuracy of middle rectangle rule should be 1`` () =
        let task = IntegrationTask(linear, segment)
        task.SolveWith <| MiddleRectangleRule m 
        |> should (equalWithin 1e-10) 1

    [<Test>]     
    member this.``Algebraic accuracy of trapezoidal rule should be 1`` () =
        let task = IntegrationTask(linear, segment)
        task.SolveWith <| TrapezoidalRule m 
        |> should (equalWithin 1e-10) 1

    [<Test>]
    member this.``Algebraic accuracy of simpson rule should be 3`` () =
        let task = IntegrationTask(cubic, segment)
        task.SolveWith <| SimpsonsRule m 
        |> should (equalWithin 1e-10) 1