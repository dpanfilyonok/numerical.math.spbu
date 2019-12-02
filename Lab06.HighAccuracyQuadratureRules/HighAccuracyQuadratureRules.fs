namespace NumericalMethods

module HighAccuracyQuadratureRules = 
    open System
    open Utils
    open ClassicalOrthogonalPolynomials
    open NonlinearEquations
    open NumericalIntegration
    open System.Runtime.InteropServices

    /// w = 1
    let compositeGaussLegendreQuadrature (f: float -> float) (segment: LineSegment) n m = 
        let normalizedSegment = {Left = -1.; Right = 1.}
        let legendrePoly = 
            legendrePolynomials
            |> Seq.take n
            |> Seq.skip (n - 2)
            |> Seq.toArray
        let nodes = EquationSolveTask(legendrePoly.[1], normalizedSegment).Solve ()
        let coefficients = 
            nodes 
            |> List.map (fun node ->
                2. * (1. - node ** 2.) / 
                (float n ** 2. * legendrePoly.[0] node ** 2.))
        let gaussianRule (segment: LineSegment) =
            [0 .. (n - 1)]
            |> List.sumBy (fun k ->
                coefficients.[k] * f (segment.Length / 2. * nodes.[k] + segment.Middle))
            |> (*) <| segment.Length / 2. 
        
        segment
        |> Segment.splitSegmentIntoEqualParts m
        |> Seq.sumBy gaussianRule   

    let gaussLegendreQuadrature (f: float -> float) (segment: LineSegment) n =
        compositeGaussLegendreQuadrature f segment n 1 

    let chebyshevGaussQuadrature (f: float -> float) n = 
        getChebyshevRoots n
        |> List.sumBy f
        |> (*) <| Math.PI / float n


    let twoNodesGaussianQuadrature (f: float -> float) (w: float -> float) (segment: LineSegment) ([<Out>] info : bool byref) = 
        let n = 2
        let moments = 
            [0 .. (2 * n - 1)]
            |> List.map (fun k -> 
                IntegrationTask((fun x -> w x * x ** float k), segment)
                    .SolveWith SimpsonsRule 10)
            |> List.toArray

        let a1 = 
            (moments.[0] * moments.[3] - moments.[2] * moments.[1]) / 
            (moments.[1] ** 2. - moments.[2] * moments.[0])
        let a2 = 
            (moments.[2] * moments.[2] - moments.[3] * moments.[1]) / 
            (moments.[1] ** 2. - moments.[2] * moments.[0])

        let (x1, x2) = 
            EquationSolveTask((fun x -> x ** 2. + a1 * x + a2), segment)
            |> Equation.solve
            |> (fun lst -> lst.[0], lst.[1])

        let aBig1 = 
            (moments.[1] - x2 * moments.[0]) / (x1 - x2)
        let aBig2 = 
            (moments.[1] - x1 * moments.[0]) / (x2 - x1)
        
        aBig1 * f x1 + aBig2 * f x2