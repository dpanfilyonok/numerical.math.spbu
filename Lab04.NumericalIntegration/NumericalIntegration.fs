namespace NumericalMethods

module NumericalIntegration = 
    open Utils
    open Utils.Segment

    /// Квадратурные формулы приближенного вычисления определенных интегралов
    type QuadratureRule = 
        | LeftRectangleRule  
        | RightRectangleRule  
        | MiddleRectangleRule  
        | TrapezoidalRule
        | SimpsonsRule
    with 
        static member GetAlgebraicAccuracyOf (rule: QuadratureRule) =
            match rule with 
            | LeftRectangleRule -> 0
            | RightRectangleRule -> 0
            | MiddleRectangleRule -> 1
            | TrapezoidalRule -> 1
            | SimpsonsRule -> 3 

    /// Задача численного интегрирования f на segment
    type IntegrationTask(f: float -> float, segment: LineSegment) = 
        let compositeLeftRectangleRule (segment: LineSegment) subintervalsCount = 
            let h = segment.Length / (float subintervalsCount)
            segment
            |> getEquidistantPoints WithLeftEndpoint subintervalsCount 
            |> List.sumBy f
            |> (*) h

        let compositeRightRectangleRule (segment: LineSegment) subintervalsCount = 
            let h = segment.Length / (float subintervalsCount)
            segment
            |> getEquidistantPoints WithRightEndpoint subintervalsCount
            |> List.sumBy f
            |> (*) h

        let compositeMiddleRectangleRule (segment: LineSegment) subintervalsCount = 
            let h = segment.Length / (float subintervalsCount)
            segment
            |> getEquidistantPoints WithLeftEndpoint subintervalsCount
            |> List.map (fun point -> point + h / 2.) 
            |> List.sumBy f
            |> (*) h
          
        let compositeTrapezoidalRule (segment: LineSegment) subintervalsCount = 
            let h = segment.Length / (float subintervalsCount)
            segment
            |> getEquidistantPoints WithoutBothEndpoints subintervalsCount
            |> List.sumBy f
            |> (+) <| (f segment.Left |> (+) <| f segment.Right) / 2.
            |> (*) h

        let compositeSimpsonsRule (segment: LineSegment) subintervalsCount =    
            let h = segment.Length / (float subintervalsCount)
            let middleCoefficients = 
                seq {
                    while true do
                        yield 4.
                        yield 2.
                } |> Seq.take (subintervalsCount - 1) |> Seq.toList
            
            segment
            |> getEquidistantPoints WithoutBothEndpoints subintervalsCount
            |> List.zip middleCoefficients
            |> List.sumBy (fun (a, x) -> a * (f x))
            |> (+) <| f segment.Left |> (+) <| f segment.Right
            |> (*) <| h / 3.

        member this.SolveWith (quadratureRule: QuadratureRule) (subintervalsCount: int) = 
            match quadratureRule with 
            | LeftRectangleRule -> compositeLeftRectangleRule segment subintervalsCount
            | RightRectangleRule -> compositeRightRectangleRule segment subintervalsCount
            | MiddleRectangleRule -> compositeMiddleRectangleRule segment subintervalsCount
            | TrapezoidalRule -> compositeTrapezoidalRule segment subintervalsCount
            | SimpsonsRule -> compositeSimpsonsRule segment (2 * subintervalsCount)
            
