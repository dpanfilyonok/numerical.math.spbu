namespace NumericalMethods

module Lab02 = 
    open System
    open AlgebraicInterpolation
    open XPlot.GoogleCharts
    open Utils

    /// Интерполируемая функция
    let f = (fun x -> 1. - Math.Exp(-x) + x ** 2.)

    /// Левый конец отрезка, содержащего узлы таблицы измерений
    let a = 0.

    /// Правый конец отрзека, содержащего узлы таблицы измерений
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
        interpolationPoints |> List.map (fun point -> point.X) |> List.iter (fun node -> printfn "%.2f" node)
        printfn "8) Значение интерполяционного многочлена в форме Лагранжа в точке x=%.2f : %.*f" y fValuePrecision plx
        printfn "9) Значение абсолютной фактической погрешнойсти для формы Лагранжа : %.*f" fValuePrecision (Math.Abs (f y - plx))
        printfn "10) Значение интерполяционного многочлена в форме Ньютона в точке x=%.2f : %.*f" y fValuePrecision pnx
        printfn "11) Значение абсолютной фактической погрешнойсти для формы Ньютона : %.*f" fValuePrecision (Math.Abs (f y - pnx))

    let getMeasuringTable () = 
        let nodes = List.init (m + 1) (fun j -> a + (float)j * (b - a) / (float)m)
        nodes
        |> List.map (fun node -> f <| node)
        |> List.zip nodes
        |> List.map (fun point -> {X = fst point; Y = snd point})

    [<EntryPoint>]
    let main argv =
        let table = getMeasuringTable ()
        let interpolationTask = InterpolationTask table |> Interpolation.withDegree n
        let interplationPoints = interpolationTask.GetInterpolationPoints y
        let lagrangePolymomial =  interpolationTask |> Interpolation.withForm LagrangePolynomial |> Interpolation.getPolynomialInNode y
        let newtownPolynomial = interpolationTask |> Interpolation.withForm NewtownPolynomial |> Interpolation.getPolynomialInNode y
        lab2output 15 m table y n interplationPoints (lagrangePolymomial y) (newtownPolynomial y) 

        let lagrangeForm = [for x in -2. .. 0.1 .. 2. -> x, lagrangePolymomial x]
        let newtownForm = [for x in -2. .. 0.1 .. 2. -> x, newtownPolynomial x]
        let originalFunction = [for x in -2. .. 0.1 .. 2. -> x, f x]
        let options = Options(curveType = "function")

        [lagrangeForm; newtownForm; originalFunction]
        |> Chart.Line
        |> Chart.WithOptions options
        |> Chart.WithLabels ["Lagrange"; "Newtown"; "Original"]
        |> Chart.Show

        0