namespace NumericalMethods

module NonlinearEquations = 
    open System
    open System.IO

    type Methods = 
        | Bisection   
        | Newton of (float -> float)
        | ModifiedNewton of (float -> float)
        | Secant

    type Section = {
        left: float
        right: float
    }

    let splitSection stepSize section = 
        Seq.unfold (fun leftBorder -> 
            if Math.Abs (leftBorder - section.right) < 1e-6 then None
            else
                let rightBoarder = min section.right (leftBorder + stepSize)
                Some ((leftBorder, rightBoarder), rightBoarder)) section.left

    let separateRoots f stepSize section = 
        splitSection stepSize section
        |> Seq.filter (fun x -> (f <| fst x) * (f <| snd x) <= 0.)
        |> Seq.map (fun (a, b) -> {left = a; right = b})

    let findRoots
            (f: float -> float) 
            (section: Section) 
            (epsilon: float) 
            (method: Methods)
            (loggerOpt: TextWriter option) =

        let outputForEachRoot methodType valuePrecision epsilon x0 stepCount xn fxn = 
            match loggerOpt with
            | None -> ()
            | Some logger ->
                let methodS = 
                    match methodType with
                    | Bisection -> "Метод бисекции"
                    | Newton _ -> "Метод Ньютона"
                    | ModifiedNewton _ -> "Модифицированный метод Ньютона"
                    | Secant -> "Метод секущих"
                logger.WriteLine (sprintf "Используемый метод : %s" methodS)
                logger.WriteLine (sprintf "Начальное приближение x_0 = %.*f" valuePrecision x0)
                logger.WriteLine (sprintf "Количество шагов для достижения необходимой точности : %i" stepCount)
                logger.WriteLine (sprintf "Приближенное решение уравнения  с точностью до %e : %.*f" epsilon valuePrecision xn)
                // logger.WriteLine (sprintf "|x_N - x_(N-1)| = %*f" valuePrecision (Math.Abs ((float)xn - xnp))) 
                logger.WriteLine (sprintf "Абсолютная величина невязки |f(x_N) - 0| =  %.*f" valuePrecision fxn)
                logger.WriteLine ()

        let bisectionMethod startApproximation = 
            let rec loop currentApproximation stepNumber = 
                let middle = (currentApproximation.right + currentApproximation.left) / 2.
                if Math.Abs (currentApproximation.right - currentApproximation.left) < 2. * epsilon then 
                    middle, (stepNumber + 1)
                else 
                    if (f currentApproximation.left) * (f middle) < 0. then 
                        loop {left = currentApproximation.left; right = middle} (stepNumber + 1)
                    else
                        loop {left = middle; right = currentApproximation.right} (stepNumber + 1)
            loop startApproximation 0

        let newtonMethod startApproximation f' = 
            let rec loop currentApproximation multiplicityFactor stepNumber = 
                try 
                    let nextApproximation = 
                        currentApproximation - 
                        multiplicityFactor * (f currentApproximation) / (f' currentApproximation)
                    if Math.Abs (nextApproximation - currentApproximation) <= epsilon then 
                        nextApproximation, (stepNumber + 1)
                    else
                        loop nextApproximation multiplicityFactor (stepNumber + 1)
                with 
                | :? DivideByZeroException -> 
                    loop currentApproximation (multiplicityFactor + 2.) stepNumber
            loop startApproximation 1. 0

        let newtonModifiedMethod startApproximation f' = 
            let derivative = f' startApproximation
            let rec loop currentApproximation stepNumber = 
                let nextApproximation = 
                    currentApproximation - (f currentApproximation) / derivative
                if Math.Abs (nextApproximation - currentApproximation) <= epsilon then 
                    nextApproximation, (stepNumber + 1)
                else
                    loop nextApproximation (stepNumber + 1)
            loop startApproximation 0

        let secantMethod startApproximation'1 startApproximation'2 = 
            let rec loop currentApproximation previousApproximation stepNumber = 
                let nextApproximation = 
                    currentApproximation - 
                    (f currentApproximation) * (currentApproximation - previousApproximation) / 
                    ((f currentApproximation) - (f previousApproximation))
                if Math.Abs (nextApproximation - currentApproximation) <= epsilon then 
                    nextApproximation, (stepNumber + 1)
                else
                    loop nextApproximation currentApproximation (stepNumber + 1)
            loop startApproximation'1 startApproximation'2 0

        section
        |> separateRoots f 1e-2
        |> Seq.map 
            (fun section -> 
                let middle = (section.left - section.right) / 2.
                let wrap method startApproximation wrappedObject = 
                    outputForEachRoot method 10 epsilon startApproximation (snd wrappedObject) (fst wrappedObject) (f <| fst wrappedObject)
                    wrappedObject
                match method with
                | Bisection -> wrap Bisection middle <| bisectionMethod section
                | Newton derivative -> wrap (Newton id) middle <| newtonMethod middle derivative
                | ModifiedNewton derivative -> wrap (ModifiedNewton id) middle <| newtonModifiedMethod middle derivative
                | Secant -> wrap Secant section.left <| secantMethod section.left section.right
            )
        |> Seq.toList
        |> List.unzip