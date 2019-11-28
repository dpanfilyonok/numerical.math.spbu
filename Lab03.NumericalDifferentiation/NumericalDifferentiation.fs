namespace NumericalMethods

module NumericalDifferentiation = 
    open Utils

    let getFirstDerivative nodeNumber (tableFunction: Point list) =
        let m =  tableFunction.Length - 1
        if nodeNumber = 0 then
            (-3. * tableFunction.[0].Y + 4. * tableFunction.[1].Y - tableFunction.[2].Y) / 
            (2. * (tableFunction.[1].X - tableFunction.[0].X))
        elif nodeNumber = m then
            (3. * tableFunction.[m].Y - 4. * tableFunction.[m - 1].Y + tableFunction.[m - 2].Y) / 
            (2. * (tableFunction.[1].X - tableFunction.[0].X))
        else
            (tableFunction.[nodeNumber + 1].Y - tableFunction.[nodeNumber - 1].Y) / 
            (2. * (tableFunction.[1].X - tableFunction.[0].X))

    let getSecondDerivative nodeNumber (tableFunction: Point list) = 
        let m =  tableFunction.Length - 1
        if nodeNumber = 0 || nodeNumber = m then
            0.
        else 
            (tableFunction.[nodeNumber + 1].Y - 2. * tableFunction.[nodeNumber].Y + tableFunction.[nodeNumber - 1].Y) /
            ((tableFunction.[1].X - tableFunction.[0].X) ** 2.)