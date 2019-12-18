namespace NumericalMethods

module ClassicalOrthogonalPolynomials = 
    open System

    /// Последовательность полиномов Лежандра на [-1; 1]
    let legendrePolynomials =
        let rec recurrenceRelation n prevPoly2 prevPoly = seq {
            yield prevPoly2
            yield! 
                fun (x: float) -> (2. * n - 1.) / n * x * (prevPoly x) - (n - 1.) / n * (prevPoly2 x)
                |> recurrenceRelation (n + 1.) prevPoly 
        }   

        recurrenceRelation 2. (fun x -> 1.) id

    /// Последовательность полиномов Чебышёва 1 рода на [-1; 1]
    let chebyshevPolynomials = 
        let rec recurrenceRelation n prevPoly2 prevPoly = seq {
            yield prevPoly2
            yield! 
                fun (x: float) -> 2. * x * (prevPoly x) - (prevPoly2 x)
                |> recurrenceRelation (n + 1.) prevPoly
        }   

        recurrenceRelation 2. (fun x -> 1.) id

    /// Последовательность приведенных полиномов Чебышёва 1 рода на [-1; 1]
    let reducedChebyshevPolynomials = 
        chebyshevPolynomials
        |> Seq.mapi (fun i f -> 
            if i < 2 then f 
            else f >> fun numerator -> numerator / 2. ** float (i - 1))

    /// Вычисляет корни полинома Чебышёва 1 рода по известным формулам
    let getChebyshevRoots n = 
        [0 .. (n - 1)]
        |> List.map (fun k -> Math.Cos (Math.PI * (float k + 0.5) / float n))

    /// Вычисляет экстремумы полинома Чебышёва 1 рода по известным формулам
    let getChebyshevExtrema n = 
        [0 .. n]
        |> List.map (fun k -> Math.Cos (Math.PI * float k / float n))

    /// Последовательность полиномов Эрмита на (-oo; +oo)
    let hermitePolynomials = 
        let rec recurrenceRelation n prevPoly2 prevPoly = seq {
            yield prevPoly2
            yield! 
                fun (x: float) -> 2. * x * (prevPoly x) - 2. * n * (prevPoly2 x)
                |> recurrenceRelation (n + 1.) prevPoly
        }   

        recurrenceRelation 1. (fun x -> 1.) (fun x -> 2. * x)

    /// Последовательность полиномов Лагерра на (0' +oo)
    let laguerrePolynomials alpha = 
        let rec recurrenceRelation n prevPoly2 prevPoly = seq {
            yield prevPoly2
            yield! 
                fun (x: float) -> ((2. * n + 1. + alpha - x) * (prevPoly x) - (n + alpha) * (prevPoly2 x)) / (n + 1.)
                |> recurrenceRelation (n + 1.) prevPoly
        }   

        recurrenceRelation 1. (fun x -> 1.) (fun x -> 1. - x)