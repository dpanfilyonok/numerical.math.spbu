namespace NumericalMethods

module NumericalIntegration = 
    open Utils
    open Utils.Segment

    /// Квадратурные формулы приближенного вычисления определенных интегралов
    type QuadratureRule = 
        | LeftRectangleRule of int  
        | RightRectangleRule of int  
        | MiddleRectangleRule of int  
        | TrapezoidalRule of int
        | SimpsonsRule of int
    with 
        static member GetAlgebraicAccuracy (rule: QuadratureRule) =
            match rule with 
            | LeftRectangleRule _ -> 0
            | RightRectangleRule _ -> 0
            | MiddleRectangleRule _ -> 1
            | TrapezoidalRule _ -> 1
            | SimpsonsRule _ -> 3 


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

        member this.SolveWith (quadratureRule: QuadratureRule) = 
            match quadratureRule with 
            | LeftRectangleRule subintervalsCount -> compositeLeftRectangleRule segment subintervalsCount
            | RightRectangleRule subintervalsCount -> compositeRightRectangleRule segment subintervalsCount
            | MiddleRectangleRule subintervalsCount -> compositeMiddleRectangleRule segment subintervalsCount
            | TrapezoidalRule subintervalsCount -> compositeTrapezoidalRule segment subintervalsCount
            | SimpsonsRule subintervalsCount -> compositeSimpsonsRule segment subintervalsCount
            