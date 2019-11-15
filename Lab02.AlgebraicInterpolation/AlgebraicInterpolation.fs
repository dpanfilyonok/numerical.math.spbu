namespace AlgebraicInterpolation

open Utils
open System

type PolynomialForm = 
    | LagrangePolynomial
    | NewtownPolynomial

/// Задача об интерполяции таблично заданной функции
type InterpolationTask(measuring: Point list) =

    let mutable polynomialForm = LagrangePolynomial
    let mutable polynomialDegree = measuring |> List.length

    let getInterpolationPoints (node: float) = 
        measuring
        |> List.sortBy (fun point -> Math.Abs (point.X - node))
        |> List.take (polynomialDegree + 1)

    let lagrangePolynomial node = 
        let interpolationPoints = getInterpolationPoints node

        (fun x ->
            let sumMemberK k x = 
                interpolationPoints
                |> Seq.mapi (fun i point -> i, point.X)
                |> Seq.filter (fun xi -> fst xi <> k)
                |> Seq.map (fun xi -> (x - snd xi) / (interpolationPoints.[k].X - snd xi))
                |> Seq.fold (*) interpolationPoints.[k].Y
            
            [0 .. polynomialDegree]
            |> Seq.sumBy (fun i -> sumMemberK i x))

    let newtownPolynomial node = 
        let interpolationPoints = getInterpolationPoints node

        let dividedDifferenceTable = Array2D.zeroCreate<float> (polynomialDegree + 1) (polynomialDegree + 1)
        
        [0 .. polynomialDegree] 
        |> Seq.iter (fun j -> dividedDifferenceTable.[0, j] <- interpolationPoints.[j].Y)
        
        Array2D.zeroCreate<bool> (polynomialDegree + 1) (polynomialDegree + 1)
        |> Array2D.iteri 
            (fun i j _ -> 
                if i > 0 && (Array2D.length1 dividedDifferenceTable - (j + i)) > 0 then 
                    dividedDifferenceTable.[i, j] <- 
                        (dividedDifferenceTable.[i - 1, j + 1] - dividedDifferenceTable.[i - 1, j]) / 
                        (interpolationPoints.[j + i].X - interpolationPoints.[j].X) 
                else ()
            )
            
        (fun x -> 
            let secondFactors = 
                List.unfold 
                    (fun (i, acc) -> 
                        if i < polynomialDegree then 
                            let newAcc = acc * (x - interpolationPoints.[i].X)
                            Some (newAcc, ((i + 1), newAcc))
                        else 
                            None
                    ) (0, 1.)

            let dividedDifference = 
                dividedDifferenceTable.[0 .. polynomialDegree, 0 .. 0] 
                |> fun array -> List.init (polynomialDegree + 1) (fun i -> array.[i, 0])

            1. :: secondFactors
            |> List.zip dividedDifference
            |> List.sumBy (fun (a, b) -> a * b)
        )            

    member this.Measuring
        with get () = measuring

    member this.PolynomialForm
        with get () = polynomialForm
        and private set value = polynomialForm <- value

    member this.PolynomialDegree
        with get () = polynomialDegree
        and private set value = polynomialDegree <- value

    member this.GetInterpolationPoints (node: float) = 
        getInterpolationPoints node

    member this.WithForm (polynomialForm: PolynomialForm) = 
        this.PolynomialForm <- polynomialForm
        this

    member this.WithDeegree (degree: int) = 
        this.PolynomialDegree <- degree
        this

    member this.GetPolynomialInNode (node: float) = 
        match polynomialForm with 
        | LagrangePolynomial -> lagrangePolynomial node
        | NewtownPolynomial -> newtownPolynomial node

    member this.ValueOf (argument: float) = 
        argument |> this.GetPolynomialInNode argument


/// Решение задачи алгебраического интерполирования многочленами в форме Лагранжа и Ньютона
module Interpolation = 
    let withDegree (degree: int) (task: InterpolationTask) = 
        task.WithDeegree degree
     
    let withForm (form: PolynomialForm) (task: InterpolationTask) = 
        task.WithForm form

    let getPolynomialInNode (node: float) (task: InterpolationTask) = 
        task.GetPolynomialInNode node

    let valueOf (argument: float) (task: InterpolationTask) = 
        task.ValueOf argument