namespace NumericalMethods

module Lab03_2 = 
    open System
    open NumericalDifferentiation
    open XPlot.GoogleCharts
    open Utils

    let f = (*) 7.5 >> Math.Exp

    let f' = f >> (*) 7.5

    let f'' = f >> (*) 56.25

    let getTable m a h = 
        let nodes = List.init (m + 1) (fun j -> a + (float j) * h)
        nodes
        |> List.map (fun node -> f <| node)
        |> List.zip nodes
        |> List.map (fun point -> {X = fst point; Y = snd point})

    let c f x y = f y x

    let lab3input () = 
        printfn "Введите значение m + 1:"
        let m = Console.ReadLine () |> int |> c (-) 1
        printfn "Введите значение a:"
        let a = Console.ReadLine () |> float
        printfn "Введите значение h>0:"
        let h = Console.ReadLine () |> float

        (m, a, h)

    [<EntryPoint>]
    let main argv =
        printfn "Нахождение производных таблично-заданной функции по формулам численного дифференцирования"
        printfn "Вариант 9"
        while true do
            let (m, a, h) = lab3input ()
            let table = getTable m a h

            let values = 
                table
                |> List.map ((fun x -> x.X, x.Y) >> (fun (x, y) -> x, y :> value))
            
            let firstDerivative = 
                table 
                |> List.mapi (fun i x -> x.X, getFirstDerivative i table)
                |> List.map (fun (x, y) -> x, y :> value)

            let firstDerivativeError = 
                table 
                |> List.mapi (fun i x -> x.X, Math.Abs (getFirstDerivative i table - f' x.X))
                |> List.map (fun (x, y) -> x, (sprintf "%.8f" y) :> value)

            let secondDerivative = 
                table 
                |> List.mapi (fun i x -> x.X, getSecondDerivative i table)
                |> List.map (fun (x, y) -> x, y :> value)

            let secondDerivativeError = 
                table 
                |> List.mapi (fun i x -> x.X, Math.Abs (getSecondDerivative i table - f'' x.X))
                |> List.map (fun (x, y) -> x, (sprintf "%.8f" y) :> value)
    
            [values; firstDerivative; firstDerivativeError; secondDerivative; secondDerivativeError]
            |> Chart.Table
            |> Chart.WithLabels ["x_i"; "Function value"; "First derivative value"; "First derivative error"; "Second derivative value"; "Second derivative error"]
            |> Chart.Show

        0
