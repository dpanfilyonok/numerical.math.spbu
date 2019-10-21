namespace NumericalMethods

module Lab04 =
    open System
    open NumericalIntegration
    open XPlot.GoogleCharts

    let f = Math.Tanh

    let a = 0.

    let b = Math.PI / 2.

    let j = 0.91995540023015

    let lab4ouput a b j =
        printfn "Приближенное вычисление интеграла по составным квадратурным формулам"
        printfn "Отрезок интегрирования [A; B] = [%.2f; %.2f]" a b
        printfn "Точное значение интеграла на промежутке J = %.2f" j

    let getTheoryError coefficient subintervalsCount algebraicAccuracy maxD = 
        coefficient 
        * (b - a)
        * ((b - a) / subintervalsCount) ** (float (algebraicAccuracy + 1))
        * maxD

    let coefficients = [2.; 2.; 24.; 12.; 2880.] |> List.map (fun x -> 1. / x)
    let maximums = [1.; 1.; 0.769800358919501; 0.769800358919501; 4.08588550296966]
    let algebraicAccuracies = [0; 0; 1; 1; 3]

    [<EntryPoint>]
    let main argv =
        lab4ouput a b j

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
                intTask.SolveWith <| SimpsonsRule m
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
                rules |> List.mapi (fun i rule -> rule, abs (j - solves.[i]) |> sprintf "%.*f" precision)

            let theoryErrors precision = 
                rules 
                |> List.mapi 
                    (fun i rule -> 
                        rule, 
                        getTheoryError coefficients.[i] (float m / 2.) algebraicAccuracies.[i] maximums.[i] 
                        |> sprintf "%.*f" precision
                    ) 

            let precision = 8 
            [solutions; errors; theoryErrors]
            |> List.map (fun f -> f precision)
            |> Chart.Table
            |> Chart.WithLabels ["Rule name"; "Approximate solution"; "Error"; "TheoryError"]
            |> Chart.Show

        0
