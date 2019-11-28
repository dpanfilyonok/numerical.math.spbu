namespace Utils

/// Точка (x, y)
type Point = {
    X: float
    Y: float
}  

module Point = 
        
    let flip (p: Point) = {X = p.Y; Y = p.X}