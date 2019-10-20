namespace Utils

module Section = 
    open System

    /// Отрезок [A; B]
    type LineSegment = {
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

    let splitSegmentWithStep stepSize segment = 
        Seq.unfold (fun leftBorder -> 
            if Math.Abs (leftBorder - segment.Right) < 1e-6 then None
            else
                let rightBoarder = min segment.Right (leftBorder + stepSize)
                Some ((leftBorder, rightBoarder), rightBoarder)) segment.Left

    let splitSegmentIntoEqualParts amountOfParts segment = 
        splitSegmentWithStep ((segment.Right - segment.Left) / (float)amountOfParts) segment