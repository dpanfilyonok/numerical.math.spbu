namespace NumericalMethods

module Lab06 = 
    open System
    open HighAccuracyQuadratureRules
    open Utils

    let f = Math.Sin
    // let w = fun (x: float) -> 1.
    let w = Math.Sqrt >> (/) 1.

    let a = -1.
    let b = 1.

    let lab6input () = 
        printfn "Введите пределы интегрирования a и b (a <= b): "
        let segment = {Left = Console.ReadLine () |> float; Right = Console.ReadLine () |> float}
        printfn "Введите число узлов для использования в квадратурных формулах (N > 0): "
        let n = Console.ReadLine () |> int
        printfn "Введите число разбиений для составной формулы Гаусса (m > 0): "
        let m = Console.ReadLine () |> int

        (segment, n, m)

    [<EntryPoint>]
    let main argv =
        printfn "Лабораторная работа №6"
        printfn "Приближённое вычисление интегралов при помощи квадратурных формул Наивысшей Алгебраической Степени Точности"

        while true do
            let (segment, n, m) = lab6input ()
            printfn "1) Интеграл при помощи КФ Гаусса = %.8f" (compositeGaussLegendreQuadrature f segment n m)
            let mutable log = None
            printfn "2) Интеграл при помощи КФ типа Гаусса = %.8f" (twoNodesGaussianQuadrature f w segment &log)
            printfn "%A" log
            printfn "3) Интеграл при помощи КФ Мелера = %.8f" (chebyshevGaussQuadrature f n)

        0