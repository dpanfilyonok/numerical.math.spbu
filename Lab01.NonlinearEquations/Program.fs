namespace NumericalMethods

module Lab01 = 
    open System
    open NonlinearEquations

    // Непрерывна на a b
    let f = (fun x -> 4. * (Math.Cos x) + 0.3 * x)

    let a = -15.

    let b = 5.

    let epsilon = 1e-5

    [<EntryPoint>]
    let main argv =
        let (roots, steps) = findRoots f {left = a; right = b} epsilon Secant (Some Console.Out)
        printfn "%A" roots
        printfn "%A" steps
        0