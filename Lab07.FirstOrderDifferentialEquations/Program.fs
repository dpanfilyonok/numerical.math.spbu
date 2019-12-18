namespace NumericalMethods

module Lab07 = 
    open System
    open Utils
    open FirstOrderDifferentialEquations

    let f = (fun x y -> -y * (2. - Math.Cos x))

    let accurateY = (fun x -> Math.Exp (Math.Sin x - 2. * x))

    let cauchyPoint = {X = 0.; Y = 1.}

    let h = 0.1

    let n = 10

    let printAccurateSolution () = 
        printfn "1) Значения точного решения в соответствующих точках: "
        [-2 .. n]
        |> List.map ((fun k -> cauchyPoint.X + float k * h) >> (fun node -> {X = node; Y = accurateY node}))
        |> List.iter (fun point -> printfn "%.1f : %.6f" point.X point.Y)
        printfn ""

    let printSolution str (points: Point list) = 
        printfn "%s" str
        points
        |> List.iter 
            (fun point -> printfn "%.3f : %.6f Погрешность: %.8f" point.X point.Y (point.Y - accurateY point.X |> Math.Abs))
        printfn ""

    let lab7input () = 
        printfn "Введите значение N (N >= 3)" 
        let n = int (Console.ReadLine())
        printfn "Введите h"
        let h = float (Console.ReadLine())
        (n, h)

    [<EntryPoint>]
    let main argv =
        printfn "Численное решение задачи Коши ОДУ 1 порядка"
        printfn "Вариант 9"
        printAccurateSolution()

        while true do 
            let (n, h) = lab7input ()
            let task = DifferentialEquationSolveTask(f, cauchyPoint)

            let derivatives = [1.; -1.; 1.; -2.; 5.]
            let taylorY = task.SolveWithTaylorSeriesExpansionMethod derivatives
            let table = 
                [-2 .. 2]
                |> List.map ((fun k -> cauchyPoint.X + float k * h) >> (fun node -> {X = node; Y = taylorY node}))

            table 
            |> printSolution "2) Значения решения, полученного методом разложения в ряд Тейлора"
            
            [3 .. n]
            |> List.scan 
                (fun state node -> 
                    let nextPoint = task.SolveWith4StepAdamsBashforthMethod state
                    state.Tail @ [nextPoint]
                ) table
            |> List.tail
            |> List.map List.last
            |> printSolution "4) Значения решения, полученного методом Адамса 4 порядка"

            let nodes = 
                [1 .. n]
                |> List.map (fun k -> cauchyPoint.X + float k * h)

            nodes 
            |> task.SolveInNodes DESolveMethod.RungeKutta4 
            |> printSolution "5) Значения решения, полученного методом Рунге-Кутта 4 порядка"

            nodes 
            |> task.SolveInNodes DESolveMethod.EulerLeftRectangle 
            |> printSolution "6.1) Значения решения, полученного методом Эйлера с использованием квадратуры левого прямоугольника"

            nodes 
            |> task.SolveInNodes DESolveMethod.EulerMiddleRectangle 
            |> printSolution "6.2) Значения решения, полученного методом Эйлера с использованием квадратуры среднего прямоугольника"

            nodes 
            |> task.SolveInNodes DESolveMethod.EulerTrapezoidal 
            |> printSolution "6.3) Значения решения, полученного методом Эйлера с использованием квадратуры трапеции"

        0