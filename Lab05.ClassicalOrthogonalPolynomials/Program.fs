namespace NumericalMethods

module Lab05 = 
    open System
    open NonlinearEquations
    open Utils
    open XPlot.GoogleCharts
    open ClassicalOrthogonalPolynomials

    let stdSegment = {Left = -1.; Right = 1.}
    let hermiteSegment = {Left = -5.; Right = 5.}
    let laguerreSegment = {Left = 0.; Right = 20.}

    let lab05input () = 
        printfn "Введите стпепень многочлена (N >= 1): "
        let n = int <| Console.ReadLine ()
        printfn "Введите параметр для многочлена Лагерра (alpha > -1): "
        let alpha = float <| Console.ReadLine ()
        (n, alpha)

    [<EntryPoint>]
    let main argv =
        while true do
            let (n, alpha) = lab05input ()

            let pn = legendrePolynomials |> Seq.item n
            [for x in stdSegment.Left .. 0.1 .. stdSegment.Right -> x, pn x]
            |> Chart.Line
            |> Chart.WithOptions (Options(curveType = "function"))
            |> Chart.WithTitle (sprintf "Многочлен Лежандра степени %i" n)
            |> Chart.Show

            let tn = chebyshevPolynomials |> Seq.item n
            [for x in stdSegment.Left .. 0.1 .. stdSegment.Right -> x, tn x]
            |> Chart.Line
            |> Chart.WithOptions (Options(curveType = "function"))
            |> Chart.WithTitle (sprintf "Многочлен Чебышева степени %i" n)
            |> Chart.Show

            let rtn = reducedChebyshevPolynomials |> Seq.item n
            [for x in stdSegment.Left .. 0.1 .. stdSegment.Right -> x, rtn x]
            |> Chart.Line
            |> Chart.WithOptions (Options(curveType = "function"))
            |> Chart.WithTitle (sprintf "Приведенный многочлен Чебышёва степени %i" n)
            |> Chart.Show

            let hn = hermitePolynomials |> Seq.item n
            [for x in hermiteSegment.Left .. 0.1 .. hermiteSegment.Right -> x, hn x]
            |> Chart.Line
            |> Chart.WithOptions (Options(curveType = "function"))
            |> Chart.WithTitle (sprintf "Многочлен Эрмита степени %i" n)
            |> Chart.Show

            let ln = laguerrePolynomials alpha |> Seq.item n
            [for x in laguerreSegment.Left .. 0.1 .. laguerreSegment.Right -> x, ln x]
            |> Chart.Line
            |> Chart.WithOptions (Options(curveType = "function"))
            |> Chart.WithTitle (sprintf "Многочлен Лагерра степени %i" n)
            |> Chart.Show

            [
                "Корни мн-на Лежандра", EquationSolveTask(pn, stdSegment).Solve () |> sprintf "%A" :> value
                "Корни мн-на Чебышёва", getChebyshevRoots n |> sprintf "%A" :> value
                "Экстремумы мн-на Чебышёва", getChebyshevExtrema n |> sprintf "%A" :> value
                "Корни мн-на Эрмита", EquationSolveTask(hn, hermiteSegment).Solve () |> sprintf "%A" :> value
                "Корни мн-на Лагерра", EquationSolveTask(ln, laguerreSegment).Solve () |> sprintf "%A" :> value
            ]
            |> Chart.Table
            |> Chart.Show

        0    