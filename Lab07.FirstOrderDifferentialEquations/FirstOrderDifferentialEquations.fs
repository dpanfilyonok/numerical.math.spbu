namespace FirstOrderDifferentialEquations

open System
open Utils

type DESolveMethod = 
    | EulerLeftRectangle
    | EulerMiddleRectangle
    | EulerTrapezoidal
    | RungeKutta4

type DifferentialEquationSolveTask(f: float -> float -> float, cauchyTask: Point) = 
    let eulerLeftMethod nodes = 
        nodes
        |> List.scan 
            (fun prevPoint currentNode -> 
                let h = currentNode - prevPoint.X
                {
                    X = currentNode 
                    Y = prevPoint.Y + h * f prevPoint.X prevPoint.Y
                }
            ) cauchyTask

    let eulerMiddleMethod nodes = 
        nodes 
        |> List.scan 
            (fun prevPoint currentNode -> 
                let h = currentNode - prevPoint.X
                let y' = prevPoint.Y + h / 2. * f prevPoint.X prevPoint.Y
                {
                    X = currentNode
                    Y = prevPoint.Y + h * f (prevPoint.X + h / 2.) y'
                }
            ) cauchyTask
    
    let eulerTrapezoidalMethod nodes = 
        nodes 
        |> List.scan 
            (fun prevPoint currentNode -> 
                let h = currentNode - prevPoint.X
                let y' = prevPoint.Y + h * f prevPoint.X prevPoint.Y
                {
                    X = currentNode
                    Y = prevPoint.Y + h / 2. * (f prevPoint.X prevPoint.Y + f currentNode y')
                }
            ) cauchyTask

    let rungeKutta nodes = 
        nodes
        |> List.scan
            (fun prevPoint currentNode -> 
                let h = currentNode - prevPoint.X
                let k1 = h * f prevPoint.X prevPoint.Y
                let k2 = h * f (prevPoint.X + h / 2.) (prevPoint.Y + k1 / 2.)
                let k3 = h * f (prevPoint.X + h / 2.) (prevPoint.Y + k2 / 2.)
                let k4 = h * f currentNode (prevPoint.Y + k3)
                {
                    X = currentNode
                    Y = prevPoint.Y + 1. / 6. * (k1 + 2. * (k2 + k3) + k4)
                }
            ) cauchyTask
            
    /// including 0 derivative
    member this.SolveWithTaylorSeriesExpansionMethod (derivatives: float list) = 
        let n = List.length derivatives
        let factorials = 
            [1 .. (n - 1)]
            |> List.scan (*) 1
            |> List.map float

        (fun x ->
            (derivatives, factorials)
            ||> List.mapi2 (fun i derivative ``i!`` ->
                derivative / ``i!`` * (x - cauchyTask.X) ** (float i))
            |> List.sum
        )

    member this.SolveWith4StepAdamsBashforthMethod (tableBegin: Point list) = 
        let k = 4
        let h = tableBegin.[1].X - tableBegin.[0].X
        let hf = tableBegin |> List.map (fun point -> h * f point.X point.Y)
        
        let coeffs = [1.; 1. / 2.; 5. / 12.; 3. / 8.; 251. / 720.]

        let c f x y = f y x
        let hfDifference = 
            let finiteDifference (nodes: float list) = 
                nodes
                |> List.windowed 2
                |> List.map (fun pairList -> pairList |> List.reduce (c (-)))
            [1 .. k]
            |> List.scan (fun state _ -> finiteDifference state) hf
            |> List.map List.last


        (coeffs, hfDifference)
        ||> List.map2 (*)
        |> List.sum
        |> (+) (List.last tableBegin).Y
        |> (fun y -> {X = (List.last tableBegin).X + h; Y = y})

    member this.SolveInNodes (solveMethod: DESolveMethod) (nodes: float list) = 
        match solveMethod with 
        | EulerLeftRectangle -> eulerLeftMethod nodes
        | EulerMiddleRectangle -> eulerMiddleMethod nodes
        | EulerTrapezoidal -> eulerTrapezoidalMethod nodes
        | RungeKutta4 -> rungeKutta nodes
        
