namespace NumericalMethods

module InverseInterpolation = 
    open AlgebraicInterpolation
    open NonlinearEquations
    open Utils.Point

    type FInfo = {
        InterpolationTask: InterpolationTask 
        IfFuncIsContinuousStrictlyMonotone: bool
        Epsilon: float option
    }

    let findInterpolationNode (info: FInfo) (polynomialDegree: int) (value: float) = 
        if info.IfFuncIsContinuousStrictlyMonotone then
            let pointsOfInverseFunction = 
                info.InterpolationTask.GetMeasuring ()
                |> List.map flip
            
            let task = InterpolationTask(pointsOfInverseFunction)
            task.LagrangePolynomial polynomialDegree (InNode value) value
        else
            