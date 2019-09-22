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

            let dividedDifferenceTable = Array2D.zeroCreate<float> (polynomialDegree + 1) (polynomialDegree + 1)
            
            [0 .. polynomialDegree] 
            |> Seq.iter (fun j -> dividedDifferenceTable.[0, j] <- interpolationPoints.[j].y)
            
            Array2D.zeroCreate<bool> (polynomialDegree + 1) (polynomialDegree + 1)
            |> Array2D.iteri 
                (fun i j _ -> 
                    if i > 0 && (Array2D.length1 dividedDifferenceTable - (j + i)) > 0 then 
                        dividedDifferenceTable.[i, j] <- 
                            (dividedDifferenceTable.[i - 1, j + 1] - dividedDifferenceTable.[i - 1, j]) / 
                            (interpolationPoints.[j + i].x - interpolationPoints.[j].x) 
                    else ()
                )
                
            let polynomial = (fun x -> 
                let secondFactors = 
                    List.unfold 
                        (fun (i, acc) -> 
                            if i < polynomialDegree then 
                                let newAcc = acc * (x - interpolationPoints.[i].x)
                                Some (newAcc, ((i + 1), newAcc))
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
            