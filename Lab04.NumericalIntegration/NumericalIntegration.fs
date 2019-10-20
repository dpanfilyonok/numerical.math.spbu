namespace NumericalMethods

module NumericalIntegration = 
    open Utils
    open Utils.Segment

    type QuadratureRule = 
        | LeftRectangle of int  
        | RightRectangle of int  
        | MiddleRectangle of int  
        | Trapezoidal of int
        | Simpsons of int

    type IntegrationTask(f: float -> float, segment: LineSegment) = 
        let compositeLeftRectangleRule (segment: LineSegment) subintervalsCount = 
            let h = segment.Length / (float subintervalsCount)
            getEquidistantPoints segment subintervalsCount WithoutRightEndpoint
            |> List.sumBy f
            |> (*) h

        let compositeRightRectangleRule (segment: LineSegment) subintervalsCount = 
            let h = segment.Length / (float subintervalsCount)
            getEquidistantPoints segment subintervalsCount WithoutLeftEndpoint
            |> List.sumBy f
            |> (*) h

        let compositeMiddleRectangleRule (segment: LineSegment) subintervalsCount = 
            let h = segment.Length / (float subintervalsCount)
            getEquidistantPoints segment subintervalsCount WithoutLeftEndpoint
            |> List.map (fun point -> point + h / 2.) 
            |> List.sumBy f
            |> (*) h
          
        let compositeTrapezoidalRule (segment: LineSegment) subintervalsCount = 
            let h = segment.Length / (float subintervalsCount)
            getEquidistantPoints segment subintervalsCount WithoutBothEndpoints
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
            
            getEquidistantPoints segment subintervalsCount WithoutBothEndpoints
            |> List.zip middleCoefficients
            |> List.sumBy (fun (a, x) -> a * (f x))
            |> (+) <| f segment.Left |> (+) <| f segment.Right
            |> (*) <| h / 3.

        member this.SolveWith (quadratureRule: QuadratureRule) = 
            match quadratureRule with 
            | LeftRectangle subintervalsCount -> compositeLeftRectangleRule segment subintervalsCount
            | RightRectangle subintervalsCount -> compositeRightRectangleRule segment subintervalsCount
            | MiddleRectangle subintervalsCount -> compositeMiddleRectangleRule segment subintervalsCount
            | Trapezoidal subintervalsCount -> compositeTrapezoidalRule segment subintervalsCount
            | Simpsons subintervalsCount -> compositeSimpsonsRule segment subintervalsCount
            