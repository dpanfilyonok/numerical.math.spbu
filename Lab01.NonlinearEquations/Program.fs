namespace NumericalMethods

module Lab01 = 
    open System
    open NonlinearEquations
    open ConsoleTables

    // Непрерывна на a b
    let f = (fun x -> 4. * (Math.Cos x) + 0.3 * x)

    let f' = (fun x -> -4. * (Math.Sin x) + 0.3)

    let a = -15.

    let b = 5.

    let epsilon = 1e-8

    let printInputData () = 
        printfn "Исходные двнные задачи:"
        printfn "Промежуток [A, B] = [%.1f, %.1f]" a b
        printfn "Корни определяются с заданной точностью e = %.0e" epsilon
        printfn ""

    let printRowNames () = 
        printfn "1) Начальное приближение к корню"
        printfn "2) Количество шагов для достижения необходимой точности"
        printfn "3) Приближенное решение уравнения"
        printfn "4) Абсолютная величина невязки приближенного решения"        
        printfn ""

    [<EntryPoint>]
    let main argv =
        printfn "Численные методы решения нелинейных уравнений"
        printfn "Вариант 8 \n"
        printInputData ()
        printRowNames ()
        
        let printMethodInfo method = 
            let (roots, info) = findRoots f { Left = a; Right = b } epsilon method
            let addRow row (table: ConsoleTable) = table.AddRow row
            let table =
                ConsoleTable (List.map (fun x -> x.Section.ToString ()) info |> List.toArray)
                |> addRow (List.map (fun x -> 
                                            (if x.StartApproximation.IsSome 
                                                then x.StartApproximation.Value.ToString () 
                                            else "-") :> obj
                                    ) info |> List.toArray) 
                |> addRow (List.map (fun x -> x.StepCount :> obj) info |> List.toArray)
                |> addRow (List.map (fun x -> x.Root :> obj) info |> List.toArray)
                |> addRow (List.map (fun x -> x.AbsoluteError :> obj) info |> List.toArray)

            table.Write(Format.Minimal)
            Console.WriteLine ()

        let methods = [
            Bisection, "Метод бисекции"
            Newton f', "Метод Ньютона"
            ModifiedNewton f', "Модфицированный метод Ньютона"
            Secant, "Метод секущих"
        ]

        methods
        |> List.iter 
            (fun (method, mName) -> 
                printfn "%s" mName
                printMethodInfo method
            ) 

        0