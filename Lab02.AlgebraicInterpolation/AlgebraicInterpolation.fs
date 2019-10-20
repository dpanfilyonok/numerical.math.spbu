namespace NumericalMethods

/// Решение задачи алгебраического интерполирования многочленами в форме Лагранжа и Ньютона
module AlgebraicInterpolation = 
    open System
    open Utils.Point

    /// Метод минимизации погрешности
    type ErrorMinimizationMethod = 
        | InNode of float
        | OnSection of float * float

    /// Задача об интерполяции таблично заданной функции
    type InterpolationTask(startingMeasuring: Point list) = 
        let mutable allMeasuring = startingMeasuring

        /// Добавляет измерения в таблицу
        member this.AddMeasuring measuring = 
            allMeasuring <- List.append allMeasuring measuring 

        /// Возвращает точки интерполяции, обеспечивающие наименьшую погрешность для заданной степени полинома
        member this.GetInterpolationPoints polynomialDegree minimizationMethod = 
            match minimizationMethod with
            | InNode x -> 
                allMeasuring
                |> List.sortBy (fun point -> Math.Abs (point.X - x))
                |> List.take (polynomialDegree + 1)
            | OnSection _ -> []

        /// Возвращает полином в форме Лагранжа заданной степени, постоенный по оптимальным узлам
        member this.LagrangePolynomial polynomialDegree minimizationMethod = 
            let interpolationPoints = this.GetInterpolationPoints polynomialDegree minimizationMethod

            (fun x ->
                let sumMemberK k x = 
                    interpolationPoints
                    |> Seq.mapi (fun i point -> i, point.X)
                    |> Seq.filter (fun xi -> fst xi <> k)
                    |> Seq.map (fun xi -> (x - snd xi) / (interpolationPoints.[k].X - snd xi))
                    |> Seq.fold (*) interpolationPoints.[k].Y
                
                [0 .. polynomialDegree]
                |> Seq.sumBy (fun i -> sumMemberK i x))

        /// Возвращает полином в форме Ньютона заданной степени, постоенный по оптимальным узлам
        member this.NewtownPolynomial polynomialDegree minimizationMethod = 
            let interpolationPoints = this.GetInterpolationPoints polynomialDegree minimizationMethod

            let dividedDifferenceTable = Array2D.zeroCreate<float> (polynomialDegree + 1) (polynomialDegree + 1)
            
            [0 .. polynomialDegree] 
            |> Seq.iter (fun j -> dividedDifferenceTable.[0, j] <- interpolationPoints.[j].Y)
            
            Array2D.zeroCreate<bool> (polynomialDegree + 1) (polynomialDegree + 1)
            |> Array2D.iteri 
                (fun i j _ -> 
                    if i > 0 && (Array2D.length1 dividedDifferenceTable - (j + i)) > 0 then 
                        dividedDifferenceTable.[i, j] <- 
                            (dividedDifferenceTable.[i - 1, j + 1] - dividedDifferenceTable.[i - 1, j]) / 
                            (interpolationPoints.[j + i].X - interpolationPoints.[j].X) 
                    else ()
                )
                
            (fun x -> 
                let secondFactors = 
                    List.unfold 
                        (fun (i, acc) -> 
                            if i < polynomialDegree then 
                                let newAcc = acc * (x - interpolationPoints.[i].X)
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