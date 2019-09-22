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
        
        let getInterpolationPoints polynomialDegree minimizationMethod = 
            match minimizationMethod with
            | InNode x -> 
                allMeasuring
                |> List.sortBy (fun point -> Math.Abs (point.x - x))
                |> List.take (polynomialDegree + 1)
            | OnSection _ -> []

        member this.AddMeasuring measuring = 
            allMeasuring <- List.append allMeasuring measuring 

        member this.LagrangePolynomialWithPoints polynomialDegree minimizationMethod = 
            let interpolationPoints = getInterpolationPoints polynomialDegree minimizationMethod
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

        member this.NewtownPolynomialWithPoints polynomialDegree minimizationMethod = 
            let interpolationPoints = getInterpolationPoints polynomialDegree minimizationMethod

            let tempTable = Array2D.zeroCreate<float> (polynomialDegree + 1) (polynomialDegree + 1)
            [0 .. polynomialDegree] 
            |> Seq.iter (fun i -> tempTable.[0, i] <- interpolationPoints.[i].y)
            let dividedDifferenceTable =                             
                tempTable
                |> Array2D.mapi 
                    (fun i j value -> 
                        if i > 0 && (Array2D.length1 tempTable - (j + i)) > 0 then 
                            (tempTable.[i - 1, j + 1] - tempTable.[i - 1, j]) / 
                            (interpolationPoints.[j + i].x - interpolationPoints.[j].x) 
                        else value
                    )

            let polynomial = (fun x -> 
                let secondFactors = 
                    List.unfold 
                        (fun (i, acc) -> 
                            if i < polynomialDegree then 
                                let m = acc * (x - interpolationPoints.[i].x)
                                Some (m, ((i + 1), m))
                            else 
                                None
                        ) (0, 1.)
    
                let dividedDifference = 
                    dividedDifferenceTable.[0 .. polynomialDegree, 0 .. 0] 
                    |> fun array -> List.init (polynomialDegree + 1) (fun i -> array.[i, 0])

                1. :: secondFactors
                |> List.zip dividedDifference
                |> List.sumBy (fun (a, b) -> a * b)
            )

            polynomial, interpolationPoints

        member this.NewtownPolynomial polynomialDegree minimizationMethod = 
            this.NewtownPolynomialWithPoints polynomialDegree minimizationMethod
            |> fst
            