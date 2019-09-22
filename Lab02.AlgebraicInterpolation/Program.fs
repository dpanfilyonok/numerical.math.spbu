namespace NumericalMethods

module Lab02 = 
    open System
    open AlgebraicInterpolation
    open XPlot.GoogleCharts

    /// Интерполируемая функция из класс MC^(n+1)[a,b]
    let f = (fun x -> 1. - Math.Exp(-x) + x ** 2.)
    /// Левый конец отрезка, содержащего узлы таблицы измерений
    let a = 0.
    /// Правы конец отрзека, содержащего узлы таблицы измерений
    let b = 1.5
    /// Степень интерполяционного многочлена (n <= m)
    let n = 7
    /// m + 1 - число значений в таблице измерений
    let m = 15
    /// Точка интерполирования, значения в которой хотим найти 
    /// (ограничений на значение нет)
    let y = 0.95

    let lab2output fValuePrecision m table y n interpolationPoints plx pnx = 
        printfn "1) Задача алгебраического интерполирования"
        printfn "2) Вариант 9"
        printfn "3) Число значений в таблице (m + 1) : %i" (m + 1)
        printfn "4) Исходная таблица значений функции :"
        table |> List.iter (fun point -> printfn "%A" point)
        printfn "6) Степень многчлена n : %i" n
        printfn "7) Набор узлов интерполяции :"
        interpolationPoints |> List.map (fun point -> point.x) |> List.iter (fun node -> printfn "%.2f" node)
        printfn "8) Значение интерполяционного многочлена в форме Лагранжа в точке x=%.2f : %.*f" y fValuePrecision plx
        printfn "9) Значение абсолютной фактической погрешнойсти для формы Лагранжа : %.*f" fValuePrecision (Math.Abs (f y - plx))
        printfn "10) Значение интерполяционного многочлена в форме Ньютона в точке x=%.2f : %.*f" y fValuePrecision pnx
        printfn "11) Значение абсолютной фактической погрешнойсти для формы Ньютона : %.*f" fValuePrecision (Math.Abs (f y - pnx))

    let getMeasuringTable () = 
        let nodes = List.init (m + 1) (fun j -> a + (float)j * (b - a) / (float)m)
        nodes
        |> List.map (fun node -> f <| node)
        |> List.zip nodes
        |> List.map (fun point -> {x = fst point; y = snd point})

    [<EntryPoint>]
    let main argv =
        let table = getMeasuringTable ()
        let task = InterpolationTask table
        let (polyL, pointsL) =  task.LagrangePolynomialWithPoints n (InNode y)
        let (polyN, pointsN) = task.NewtownPolynomialWithPoints n (InNode y)
        lab2output 8 m table y n pointsL (polyL y) (polyN y) 
        printfn "%.8f" (f y)
        pointsN |> List.map (fun point -> point.x) |> List.iter (fun node -> printfn "%.2f" node)
        let lagr = [for x in a .. 0.001 .. b -> x, polyL x]
        let newt = [for x in a .. 0.001 .. b -> x, polyN x]
        let f = [for x in a .. 0.001 .. b -> x, f x]
        let options = Options(
                        curveType = "function",
                        legend = Legend(position = "bottom")
                        )
        [lagr; newt; f]
        |> Chart.Line
        |> Chart.WithOptions options
        |> Chart.WithLabels ["lagr"; "newt"; "f"]
        |> Chart.Show

        0