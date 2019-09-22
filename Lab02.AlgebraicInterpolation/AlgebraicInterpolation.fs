namespace NumericalMethods

module AlgebraicInterpolation = 
    open System

    type InterpolationPoint = {
        x: float
        y: float
    }

    type ErrorMinimizationMethod = 
        | InNode of float
        | OnSection of float * float

    type InterpolationTask(startingMeasuring: InterpolationPoint list) = 
        let mutable allMeasuring = startingMeasuring
        
        member this.AddMeasuring measuring = 
            allMeasuring <- List.append allMeasuring measuring 

        member this.LagrangePolynomialWithPoints polynomialDegree minimizationMethod = 
            let interpolationPoints =       
                match minimizationMethod with
                | InNode x -> 
                    allMeasuring
                    |> List.sortBy (fun point -> Math.Abs (point.x - x))
                    |> List.take (polynomialDegree + 1)
                | OnSection _ -> []

            let polynomial = (fun x ->
                let sumMemberK k x = 
                    interpolationPoints
                    |> Seq.mapi (fun i point -> i, point.x)
                    |> Seq.filter (fun xi -> fst xi <> k)
                    |> Seq.map (fun xi -> (x - snd xi) / (interpolationPoints.[k].x - snd xi))
                    |> Seq.fold (*) interpolationPoints.[k].y
                
                [0 .. polynomialDegree]
                |> Seq.sumBy (fun i -> sumMemberK i x))

            polynomial, interpolationPoints

        member this.LagrangePolynomial polynomialDegree minimizationMethod = 
            this.LagrangePolynomialWithPoints polynomialDegree minimizationMethod
            |> fst

        member this.NewtownPolynomial x = ()