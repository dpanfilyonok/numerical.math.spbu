namespace Utils

module Section = 
    open System

    /// Отрезок [A; B]
    type Section = {
        Left: float
        Right: float
    }
    with 
        static member Default = {
            Left = 0.
            Right = 1. 
        } 
        override this.ToString () = 
            sprintf "[%0.2f , %0.2f]" this.Left this.Right

    let splitSection stepSize section = 
        Seq.unfold (fun leftBorder -> 
            if Math.Abs (leftBorder - section.Right) < 1e-6 then None
            else
                let rightBoarder = min section.Right (leftBorder + stepSize)
                Some ((leftBorder, rightBoarder), rightBoarder)) section.Left