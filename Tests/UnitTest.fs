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
    let hyperCubic = fun (x: float) -> 5. * x ** 4.
    
    let segment = {Left = 0.; Right = 1.}

    let m = 5

    [<Test>]
    member this.``Algebraic accuracy of left rectangle rule should be 0`` () =
        let rule = LeftRectangleRule

        let taskAccurate = IntegrationTask(constant, segment)
        taskAccurate.SolveWith rule m 
        |> should (equalWithin Constants.Ypsilon) (QuadratureRule.GetAlgebraicAccuracyOf rule + 1)

        let taskInaccurate = IntegrationTask(linear, segment)
        taskInaccurate.SolveWith rule m 
        |> should not' ((equalWithin Constants.Ypsilon) 1)

    [<Test>]
    member this.``Algebraic accuracy of right rectangle rule should be 0`` () =
        let rule = RightRectangleRule

        let taskAccurate = IntegrationTask(constant, segment)
        taskAccurate.SolveWith rule m 
        |> should (equalWithin Constants.Ypsilon) (QuadratureRule.GetAlgebraicAccuracyOf rule + 1)

        let taskInaccurate = IntegrationTask(linear, segment)
        taskInaccurate.SolveWith rule m 
        |> should not' ((equalWithin Constants.Ypsilon) 1)

    [<Test>]
    member this.``Algebraic accuracy of middle rectangle rule should be 1`` () =
        let rule = MiddleRectangleRule

        let f = (fun x -> [constant; linear] |> List.sumBy (fun f -> f x))
        let taskAccurate = IntegrationTask(f, segment)
        taskAccurate.SolveWith rule m 
        |> should (equalWithin Constants.Ypsilon) (QuadratureRule.GetAlgebraicAccuracyOf rule + 1)

        let taskInaccurate = IntegrationTask(quadratic, segment)
        taskInaccurate.SolveWith rule m 
        |> should not' ((equalWithin Constants.Ypsilon) 1)

    [<Test>]     
    member this.``Algebraic accuracy of trapezoidal rule should be 1`` () =
        let rule = TrapezoidalRule

        let f = (fun x -> [constant; linear] |> List.sumBy (fun f -> f x))
        let taskAccurate = IntegrationTask(f, segment)
        taskAccurate.SolveWith rule m 
        |> should (equalWithin Constants.Ypsilon) (QuadratureRule.GetAlgebraicAccuracyOf rule + 1)

        let taskInaccurate = IntegrationTask(quadratic, segment)
        taskInaccurate.SolveWith rule m 
        |> should not' ((equalWithin Constants.Ypsilon) 1)

    [<Test>]
    member this.``Algebraic accuracy of simpson rule should be 3`` () =
        let rule = SimpsonsRule

        let f = (fun x -> [constant; linear; quadratic; cubic] |> List.sumBy (fun f -> f x))
        let taskAccurate = IntegrationTask(f, segment)
        taskAccurate.SolveWith rule m 
        |> should (equalWithin Constants.Ypsilon) (QuadratureRule.GetAlgebraicAccuracyOf rule + 1)

        let taskInaccurate = IntegrationTask(hyperCubic, segment)
        taskInaccurate.SolveWith rule m 
        |> should not' ((equalWithin Constants.Ypsilon) 1)