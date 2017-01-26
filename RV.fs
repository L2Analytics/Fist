module Fist.RV

open MathNet.Numerics.Distributions
open MathNet.Numerics.Random


let sample (x: RV<'a>) = x.Sample ()
let logDensity (x: RV<'a>) (a: 'a) = x.LogDensity a

(*
let (>>=) (f: 'a -> RV<'b>) (x: RV<'a>) = 
    {new RV<'b> with
        member this.Sample () = (sample x) |> f |> sample
        member this.LogDensity b = logDensity x}
*)

(*
let (<*>) (f: RV<'a -> 'b>) (x: RV<'a>) = 
    {new RV<'b> with
        member this.Sample () = sample x |> (sample f)
        member this.LogDensity b = }
*)

let Constant (x: 'a) =
    {new RV<'a> with
        member this.Sample () = x
        member this.LogDensity _ = 0.0}

//Does *NOT* incorporate Jacobian of transformation-- use only for, e.g.,
//  converting tuples into records
let reshape (f: Bijection<'a, 'b>) (x: RV<'a>) =
    {new RV<'b> with
        member this.Sample () = x |> sample |> f.Fun
        member this.LogDensity b = b |> f.Inv |> (logDensity x)}

//Like bind, but needs to return the joint RV--if we just returned RV<'b>
//  then we couldn't compute density analytically
let conditional (f: 'a -> RV<'b>) (x: RV<'a>) :RV<'a*'b>=
    {new RV<'a*'b> with
        member this.Sample () =
            let a = sample x
            let b = a |> f |> sample
            (a, b)
        member this.LogDensity ((a, b): 'a*'b) = (logDensity x a) + (logDensity (f a) b)
        }

let indep (x: RV<'b>) (y: RV<'a>) :RV<'a*'b>=
    {new RV<'a*'b> with
        member this.Sample () = (sample y, sample x)
        member this.LogDensity ((a, b): 'a*'b) = (logDensity y a) + (logDensity x b)}

let (=|=) = indep

(*
//Clean this up
let foldConditional (xs: ('a -> RV<'a>) list) (start: RV<'a>) =
    {new RV<'a list> with
        member this.Sample () = List.fold (fun state next -> ((sample (next (List.head state)))::state) (sample start) (List.rev xs))
        member this.LogDensity zs =
            List.zip xs zs
            |> List.fold (fun state next -> state
*)

let indepList (x: list<RV<'a>>) :RV<'a list> =
    {new RV<'a list> with
        member this.Sample () = List.map sample x
        member this.LogDensity a = List.map2 logDensity x a |> List.sum}

type RVBuilder() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Some x

let sampleWeighted (x: ('a*float) array) =
    let C = new Categorical (Array.map snd x)
    fst x.[C.Sample()]

(*
let Discrete (x: ('a*float) array) =
    let C = new Categorical (Array.map snd x)
    {new RV<'a> with
        member this.Sample () = fst x.[C.Sample()]
        member this.LogDensity z = 0.5}
*)

(*
let Discrete (x: ('a*float) array) =
    let tabulated =
        x
        |> Array.groupBy fst
        |> Array.map (fun (a, aps) -> (a, aps |> (Array.map snd) |> Array.sum))
    let pmf = Map.ofArray tabulated
    let C = new Categorical (Array.map snd tabulated)
    {new RV<'a> with
        member this.Sample () = fst tabulated.[C.Sample()]
        member this.LogDensity z =
            match Map.tryFind z pmf with
            | Some p -> p
            | None -> 0.0}
*)


let Gaussian (mean: float) (stddev: float) =
    let Z = new Normal(mean, stddev)
    {new RV<float> with
        member this.Sample () = Z.Sample ()
        member this.LogDensity z = Z.DensityLn z}

let Exponential (lambda: float) =
    let T = new Exponential(lambda)
    {new RV<float> with
        member this.Sample () = T.Sample ()
        member this.LogDensity t = T.DensityLn t}

let x = new DiscreteUniform (0, 100)