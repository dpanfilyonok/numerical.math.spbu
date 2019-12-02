namespace NumericalMethods

module HighAccuracyQuadratureRules = 
    open System
    open Utils
    open ClassicalOrthogonalPolynomials
    open NonlinearEquations
    open NumericalIntegration
    open System.Runtime.InteropServices
    open MathNet.Numerics.Integration

    let twoNodesGaussianQuadrature (f: float -> float) (w: float -> float) (segment: LineSegment) 
            ([<Out>] log: 
            {|  moments: (float * float * float * float)
                a1a2: (float * float)
                nodes: (float * float) 
                coeffs: (float * float) 
            |} option byref) = 

        let n = 2
        let moments = 
            [0 .. (2 * n - 1)]
            |> List.map (fun k -> 
                // IntegrationTask((fun x -> w x * x ** float k), segment)
                //     .SolveWith SimpsonsRule 10)
                GaussLegendreRule.Integrate((fun x -> w x * x ** float k), segment.Left, segment.Right, 10)) // ???
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
            |> fun lst -> lst.[0], lst.[1]

        let coeff1 = (moments.[1] - x2 * moments.[0]) / (x1 - x2)
        let coeff2 = (moments.[1] - x1 * moments.[0]) / (x2 - x1)
        
        let methodLog =
            {| 
                moments = (moments.[0], moments.[1], moments.[2], moments.[3])
                a1a2 = (a1, a2)
                nodes = (x1, x2)
                coeffs = (coeff1, coeff2)
            |} |> Some

        log <- methodLog
        coeff1 * f x1 + coeff2 * f x2

    /// w = 1
    let compositeGaussLegendreQuadrature (f: float -> float) (segment: LineSegment) n m = 
        let normalizedSegment = {Left = -1.; Right = 1.}

        let (previousPolynomial, nthPolynomial) = 
            legendrePolynomials
            |> Seq.take (n + 1)
            |> Seq.skip (n - 1)
            |> Seq.toArray
            |> fun arr -> (arr.[0], arr.[1])

        let nodes = 
            EquationSolveTask(nthPolynomial, normalizedSegment)
            |> Equation.solve

        let coefficients = 
            nodes 
            |> List.map (fun node ->
                2. * (1. - node ** 2.) / 
                (float n ** 2. * previousPolynomial node ** 2.))
                
        let gaussLegendreQuadrature (segment: LineSegment) =
            [0 .. (n - 1)]
            |> List.sumBy (fun k ->
                coefficients.[k] * f (segment.Length / 2. * nodes.[k] + segment.Middle))
            |> (*) <| segment.Length / 2. 
        
        segment
        |> Segment.splitSegmentIntoEqualParts m
        |> Seq.sumBy gaussLegendreQuadrature   

    let gaussLegendreQuadrature (f: float -> float) (segment: LineSegment) n =
        compositeGaussLegendreQuadrature f segment n 1 

    let chebyshevGaussQuadrature (f: float -> float) n = 
        getChebyshevRoots n
        |> List.sumBy f
        |> (*) <| Math.PI / float n