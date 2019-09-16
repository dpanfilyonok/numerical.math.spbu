namespace Lab01.NonlinearEquations

module NonlinearEquations = 
    open System

    type Methods = 
        | Bisection   
        | Newton
        | ModifiedNewton
        | Secant

    type Section = {
        left: float
        right: float
    }

    let splitSection (section: Section) (h: float) = 
        Seq.unfold (fun leftBorder -> 
            if Math.Abs (leftBorder - section.right) < 1e-6 then None
            else
                let rightBoarder = min section.right (leftBorder + h)
                Some ((leftBorder, rightBoarder), rightBoarder)) section.left

    let separateRoots (f: float -> float) (section: Section) h = 
        splitSection section h
        |> Seq.filter (fun x -> (f <| fst x) * (f <| snd x) <= 0.)

    let solveEq 
            (f: float -> float) 
            (f': float -> float) 
            (section: Section) 
            (epsilon: float) 
            (method: Methods) = 
        let rec bisection (section: Section) = 
            if Math.Abs (section.right - section.left) < 2. * epsilon then 
                (section.right - section.left) / 2.
            else 
                let middle = (section.right - section.left) / 2.
                if (f section.left) * (f middle) < 0. then 
                    bisection {left = section.left; right = middle}
                else
                    bisection {left = middle; right = section.right}
        let newton = ()

        match method with
        | Bisection -> ()
        | Newton
        | ModifiedNewton -> ()
        | Secant -> ()
    