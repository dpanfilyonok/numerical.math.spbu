namespace Utils

/// Отрезок [A; B]
type LineSegment = {
    Left: float
    Right: float
} with 
    static member Default = {
        Left = 0.
        Right = 1. 
    } 
    
    member this.Length = 
        this.Right - this.Left
        
    member this.Middle = 
        (this.Left + this.Right) / 2.

    override this.ToString () = 
        sprintf "[%0.2f , %0.2f]" this.Left this.Right

module Segment = 
    open System

    type SegmentType = 
        | WithRightEndpoint
        | WithLeftEndpoint
        | WithBothEndpoints
        | WithoutBothEndpoints

    let splitSegmentWithStep stepSize segment = 
        Seq.unfold 
            (fun leftBorder -> 
                if Math.Abs (leftBorder - segment.Right) < 1e-10 then None
                else
                    let rightBoarder = min segment.Right (leftBorder + stepSize)
                    Some ({Left = leftBorder; Right = rightBoarder}, rightBoarder)
            ) segment.Left

    let splitSegmentIntoEqualParts amountOfParts segment = 
        splitSegmentWithStep ((segment.Right - segment.Left) / (float)amountOfParts) segment

    let getEquidistantPoints sType amountOfParts (segment: LineSegment) = 
        let bothEndpoint = 
            splitSegmentIntoEqualParts amountOfParts segment
            |> Seq.map (fun segment -> segment.Right)
            |> Seq.toList
            |> (fun tail -> segment.Left :: tail)
        let withoutLeft = lazy bothEndpoint.Tail
        let withoutRight = lazy List.take (bothEndpoint.Length - 1) bothEndpoint
        let withoutBoth = lazy withoutRight.Value.Tail

        match sType with 
        | WithBothEndpoints -> bothEndpoint
        | WithLeftEndpoint -> withoutRight.Value
        | WithRightEndpoint -> withoutLeft.Value
        | WithoutBothEndpoints -> withoutBoth.Value
