namespace NumericalMethods

module ClassicalOrthogonalPolynomials = 
    open System

    /// Последовательность полиномов Лежандра на [-1; 1]
    let legendrePolynomials =
        let rec recurrenceRelation n p2 p1 = seq {
            yield p2
            yield! 
                fun (x: float) -> (2. * n - 1.) / n * x * (p1 x) - (n - 1.) / n * (p2 x)
                |> recurrenceRelation (n + 1.) p1 
        }   

        recurrenceRelation 2. (fun x -> 1.) id

    /// Последовательность полиномов Чебышёва 1 рода на [-1; 1]
    let chebyshevPolynomials = 
        let rec recurrenceRelation n p2 p1 = seq {
            yield p2
            yield! 
                fun (x: float) -> 2. * x * (p1 x) - (p2 x)
                |> recurrenceRelation (n + 1.) p1
        }   

        recurrenceRelation 2. (fun x -> 1.) id

    /// Последовательность приведенных полиномов Чебышёва 1 рода на [-1; 1]
    let reducedChebyshevPolynomials = 
        let rec recurrenceRelation n p2 p1 = seq {
            yield p2
            yield! 
                fun (x: float) -> x * (p1 x) - (p2 x) / 2.
                |> recurrenceRelation (n + 1.) p1
        }   

        recurrenceRelation 2. (fun x -> 1.) id

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
        let rec recurrenceRelation n p2 p1 = seq {
            yield p2
            yield! 
                fun (x: float) -> 2. * x * (p1 x) - 2. * n * (p2 x)
                |> recurrenceRelation (n + 1.) p1
        }   

        recurrenceRelation 1. (fun x -> 1.) (fun x -> 2. * x)

    /// Последовательность полиномов Лагерра на (0' +oo)
    let laguerrePolynomials alpha = 
        let rec recurrenceRelation n p2 p1 = seq {
            yield p2
            yield! 
                fun (x: float) -> ((2. * n + 1. + alpha - x) * (p1 x) - (n + alpha) * (p2 x)) / (n + 1.)
                |> recurrenceRelation (n + 1.) p1
        }   

        recurrenceRelation 1. (fun x -> 1.) (fun x -> 1. - x)