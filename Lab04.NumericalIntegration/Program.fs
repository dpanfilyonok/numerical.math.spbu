namespace NumericalMethods

module Lab04 =
    open System
    open NumericalIntegration
    open XPlot.GoogleCharts

    /// Интегрируемая функция (непрерывна и конечна на [a;b])
    let f = Math.Tanh

    /// Первообразная функции f
    let antiderivativeF = Math.Cosh >> Math.Log

    /// Левый конец отрезка интегрирования
    let a = 0.

    /// Правый конец отрезка интегрирования
    let b = Math.PI / 2.

    /// Значение определенного интеграла по формуле Ньютона-Лейбница
    let integralValue = (antiderivativeF b, antiderivativeF a) ||> (-)

    /// Формула вычисления максимальной теоретической погрешности
    let getTheoryError coefficient subintervalsCount algebraicAccuracy maxD = 
        coefficient 
        * (b - a)
        * ((b - a) / subintervalsCount) ** (float (algebraicAccuracy + 1))
        * maxD

    let coefficients = [2.; 2.; 24.; 12.; 2880.] |> List.map (fun x -> 1. / x)
    let maximums = [1.; 1.; 0.769800358919501; 0.769800358919501; 4.08588550296966]
    let algebraicAccuracies = [0; 0; 1; 1; 3]

    /// Консольный вывод лабораторной работы 4
    let lab4ouput a b j =
        printfn "Приближенное вычисление интеграла по составным квадратурным формулам"
        printfn "Отрезок интегрирования [A; B] = [%.2f; %.2f]" a b
        printfn "Точное значение интеграла на промежутке J = %.2f" j

    [<EntryPoint>]
    let main argv =
        lab4ouput a b integralValue

        let intTask = IntegrationTask(f, {Left = a; Right = b})
        let mutable m = 0
        while true do
            printfn "Введите параметр m: "
            m <- Convert.ToInt32 (Console.ReadLine ())

            let solves = [|
                intTask.SolveWith <| LeftRectangleRule m
                intTask.SolveWith <| RightRectangleRule m
                intTask.SolveWith <| MiddleRectangleRule m
                intTask.SolveWith <| TrapezoidalRule m
                intTask.SolveWith <| SimpsonsRule (2 * m)
            |]

            let rules = [
                "Left rectangle"
                "Right rectangle"
                "Middle rectangle"
                "Trapezoidal"
                "Simpson`s"
            ]

            let solutions precision = 
                rules |> List.mapi (fun i rule -> rule, solves.[i] |> sprintf "%.*f" precision)
                
            let errors precision= 
                rules |> List.mapi (fun i rule -> rule, abs (integralValue - solves.[i]) |> sprintf "%.*f" precision)

            let theoryErrors precision = 
                rules 
                |> List.mapi 
                    (fun i rule -> 
                        rule, 
                        getTheoryError coefficients.[i] (float m) algebraicAccuracies.[i] maximums.[i] 
                        |> sprintf "%.*f" precision
                    ) 

            let precision = 8
            [solutions; errors; theoryErrors]
            |> List.map (fun f -> f precision)
            |> Chart.Table
            |> Chart.WithLabels ["Rule name"; "Approximate solution"; "Error"; "TheoryError"]
            |> Chart.Show

        0
