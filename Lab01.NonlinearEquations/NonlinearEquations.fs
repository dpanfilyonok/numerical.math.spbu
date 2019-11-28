namespace NonlinearEquations

open Utils
open System
open Utils.Segment

type SolveMethod = 
    | Bisection   
    | Newton of (float -> float)
    | ModifiedNewton of (float -> float)
    | Secant

type MethodInfo = {
    mutable Segment: LineSegment
    mutable StartApproximation: float option
    mutable StepCount: int
    mutable Root: float
    mutable AbsoluteError: float
} with 
    static member Default = {
        Segment = LineSegment.Default
        StartApproximation = None
        StepCount = 0
        Root = 0.
        AbsoluteError = 0.
    }

type EquationSolveTask(func: float -> float, segment: LineSegment) = 

    let mutable solveMethod = Secant
    let mutable accuracy = 1e-10

    let separateRoots f stepSize section = 
        splitSegmentWithStep stepSize section
        |> Seq.filter (fun x -> (f <| x.Left) * (f <| x.Right) <= 0.)

    let bisectionMethod startApproximation = 
        let rec loop currentApproximation stepNumber = 
            let middle = (currentApproximation.Right + currentApproximation.Left) / 2.
            if Math.Abs (currentApproximation.Right - currentApproximation.Left) < 2. * accuracy then 
                middle, (stepNumber + 1)
            else 
                if (func currentApproximation.Left) * (func middle) < 0. then 
                    loop {Left = currentApproximation.Left; Right = middle} (stepNumber + 1)
                else
                    loop {Left = middle; Right = currentApproximation.Right} (stepNumber + 1)
        loop startApproximation 0

    let newtonMethod startApproximation f' = 
        let rec loop currentApproximation multiplicityFactor stepNumber = 
            try 
                let nextApproximation = 
                    currentApproximation - 
                    multiplicityFactor * (func currentApproximation) / (f' currentApproximation)
                if Math.Abs (nextApproximation - currentApproximation) <= accuracy then 
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
                currentApproximation - (func currentApproximation) / derivative
            if Math.Abs (nextApproximation - currentApproximation) <= accuracy then 
                nextApproximation, (stepNumber + 1)
            else
                loop nextApproximation (stepNumber + 1)
        loop startApproximation 0

    let secantMethod startApproximation'1 startApproximation'2 = 
        let rec loop currentApproximation previousApproximation stepNumber = 
            let nextApproximation = 
                currentApproximation - 
                (func currentApproximation) * (currentApproximation - previousApproximation) / 
                ((func currentApproximation) - (func previousApproximation))
            if Math.Abs (nextApproximation - currentApproximation) <= accuracy then 
                nextApproximation, (stepNumber + 1)
            else
                loop nextApproximation currentApproximation (stepNumber + 1)
        loop startApproximation'1 startApproximation'2 0

    member this.SolveMethod 
        with get () = solveMethod
        and private set value = solveMethod <- value

    member this.Accuracy 
        with get () = accuracy
        and private set value = accuracy <- value

    member this.WithSolveMethod (method: SolveMethod) = 
        solveMethod <- method
        this

    member this.WithAccuracy (accuracy: float) = 
        this.Accuracy <- accuracy
        this

    member this.SolveWithInfo () = 
        segment
        |> separateRoots func 1e-2
        |> Seq.map 
            (fun section -> 
                let middle = (section.Left + section.Right) / 2.
                let logAndReturn startApproximation compMethodResult = 
                    let logger = MethodInfo.Default
                    logger.Segment <- section
                    logger.StartApproximation <- startApproximation
                    logger.StepCount <- snd compMethodResult
                    logger.Root <- fst compMethodResult
                    logger.AbsoluteError <- Math.Abs (func <| fst compMethodResult)
                    fst compMethodResult, logger

                match solveMethod with
                | Bisection -> logAndReturn None <| bisectionMethod section
                | Newton derivative -> logAndReturn (Some middle) <| newtonMethod middle derivative
                | ModifiedNewton derivative -> logAndReturn (Some middle) <| newtonModifiedMethod middle derivative
                | Secant -> logAndReturn None <| secantMethod section.Left section.Right
            )
        |> Seq.toList
        |> List.unzip

    member this.Solve () = 
        this.SolveWithInfo () |> fst

module Equation = 
    let withSolveMethod (method: SolveMethod) (equation: EquationSolveTask) =
        equation.WithSolveMethod method

    let withAccuracy (accuracy: float) (equation: EquationSolveTask) =
        equation.WithAccuracy accuracy

    let solveWithInfo (equation: EquationSolveTask) = 
        equation.SolveWithInfo ()

    let solve (equation: EquationSolveTask) = 
        equation.Solve ()