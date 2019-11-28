namespace NumericalMethods

module InverseInterpolation = 
    open AlgebraicInterpolation
    open NonlinearEquations
    open Utils.Point
    open Utils

    type FInfo = {
        InterpolationTask: InterpolationTask 
        IfFuncIsContinuousStrictlyMonotone: bool
        Accuracy: float option
    }

    let findInterpolationNodes (value: float) (info: FInfo) = 
        if info.IfFuncIsContinuousStrictlyMonotone 
        then
            let pointsOfInverseFunction = 
                info.InterpolationTask.Measuring
                |> List.map flip
            
            InterpolationTask(pointsOfInverseFunction)
            |> Interpolation.withDegree info.InterpolationTask.PolynomialDegree
            |> Interpolation.valueOf value
            |> List.singleton
        else
            // каждый раз заново вычисляется - ИСПРАВИТЬ
            let func = fun x -> info.InterpolationTask.ValueOf x - value
            let nodes = info.InterpolationTask.Measuring |> List.map (fun x -> x.X)

            EquationSolveTask(func, {Left = nodes |> List.min; Right = nodes |> List.max})
            |> Equation.withSolveMethod Bisection
            |> Equation.withAccuracy info.Accuracy.Value
            |> Equation.solve