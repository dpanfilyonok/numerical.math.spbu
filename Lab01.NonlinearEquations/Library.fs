namespace Lab01.NonlinearEquations

module NonlinearEquations = 
    open System

    type Methods = 
        | Bisection
        | SimpleIteration
        | Newton
        | Secant


    let splitSection (a: float) (b: float) (h: float) = 
        Seq.unfold (fun leftBorder -> 
            if Math.Abs (leftBorder - b) < 1e-6 then None
            else
                let rightBoarder = min b (leftBorder + h)
                Some ((leftBorder, rightBoarder), rightBoarder)) a

    let separateRoots (f: float -> float) a b h = 
        splitSection a b h
        |> Seq.filter (fun x -> (f <| fst x) * (f <| snd x) <= 0.)

    let solveEq (method: Methods) = 
        match method with
        | Bisection -> 
        
    
    let s = solveEq Methods.Newton