namespace NumericalMethods

module Lab03_1 = 
    open System
    open InverseInterpolation
    open AlgebraicInterpolation
    open Utils

    let f = (fun x -> 1. - Math.Exp(-x) + x ** 2.)

    let a = 0.

    let b = 1.

    // m+1 - число значений в таблице
    let m = 10

    let epsilon = 1e-8

    let lab3input () = 
        printfn "Введите значение F: "
        let fValue = Convert.ToDouble (Console.ReadLine ())
        printfn "Введите значение n: "
        let n = Convert.ToInt32 (Console.ReadLine ())
        printfn "Введите значение epsilon: "
        let epsilon = Convert.ToDouble (Console.ReadLine ())

        (fValue, n, epsilon)

    let printTable fValue nodes =
        printfn "Значения аргумента - значение модуля невязки" 
        nodes
        |> List.iter (fun node -> printfn "%.8f - %.8f" node (Math.Abs (f node - fValue)))

    let getMeasuringTable () = 
        let nodes = List.init (m + 1) (fun j -> a + (float j) * (b - a) / (float m))
        nodes
        |> List.map (fun node -> f <| node)
        |> List.zip nodes
        |> List.map (fun point -> {X = fst point; Y = snd point})

    let lab3output () = 
        printfn "Решение задачи обратного интерполирования на отрезке [%.2f; %.2f]" a b
        printfn "Таблица пар (точка - значение)"
        let table = getMeasuringTable ()
        table
        |> List.iter (fun point ->
            printfn "%.2f : %.8f" point.X point.Y)

    [<EntryPoint>]
    let main argv =
        lab3output ()

        let task = InterpolationTask(getMeasuringTable ())
        
        while true do
            let (fValue, n, epsilon) = lab3input ()
            let newTask = 
                task
                |> Interpolation.withDegree n

            printfn "\nСпособ 1"
            {
                InterpolationTask = newTask
                IfFuncIsContinuousStrictlyMonotone = true
                Accuracy = None
            } 
            |> findInterpolationNodes fValue
            |> printTable fValue

            printfn "\nСпособ 2"
            {
                InterpolationTask = newTask
                IfFuncIsContinuousStrictlyMonotone = false
                Accuracy = Some epsilon
            } 
            |> findInterpolationNodes fValue
            |> printTable fValue

        0

