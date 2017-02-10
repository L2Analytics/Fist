module Fist.RV

open MathNet.Numerics.Distributions
open MathNet.Numerics.Random

let gen = Random.shared

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


let indepMap (m: Map<'a, RV<'b>>) :RV<Map<'a, 'b>> =
    {new RV<Map<'a, 'b>> with
        member this.Sample () =
            Map.map (fun (a:'a) (b:RV<'b>) -> b.Sample ()) m
        member this.LogDensity x =
            m
            |> Map.toArray
            |> Array.map snd
            |> Array.zip (Map.toArray x |> Array.map snd)
            |> Array.map (fun (x, rv) -> rv.LogDensity x)
            |> Array.sum}

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

let resample (n: int) (x: ('a*float) array) : array<'a*float> =
    let C = new Categorical (Array.map snd x)
    Array.init n (fun _ -> (fst x.[C.Sample()]))
    |> Utilities.equalWeight


let sampleWeighted (x: ('a*float) array) =
    let C = new Categorical((Array.map snd x), gen)
    fst x.[C.Sample()]

(*
let Discrete (x: ('a*float) array) =
    let C = new Categorical (Array.map snd x)
    {new RV<'a> with
        member this.Sample () = fst x.[C.Sample()]
        member this.LogDensity z = 0.5}
*)


let Discrete (x: ('a*float) array) =
    let tabulated =
        x
        |> Array.groupBy fst
        |> Array.map (fun (a, aps) -> (a, aps |> (Array.map snd) |> Array.sum))
    let pmf = Map.ofArray tabulated
    let C = new Categorical((Array.map snd tabulated), gen)
    {new RV<'a> with
        member this.Sample () = fst tabulated.[C.Sample()]
 //Note the issue with probability zero events...
        member this.LogDensity z =
            match Map.tryFind z pmf with
            | Some p -> log p
            | None -> -infinity}

let Gaussian (mean: float) (stddev: float) =
    let Z = new Normal(mean, stddev, gen)
    {new RV<float> with
        member this.Sample () = Z.Sample ()
        member this.LogDensity z = Z.DensityLn z}

//Think about a sensible way to allow composition with Discrete
//  for sure it could be done if Discrete also had a method Support()
let smoothed (x: (float*float) array) (h: float) =
    let tabulated =
        x
        |> Array.groupBy fst
        |> Array.map (fun (a, aps) -> (a, aps |> (Array.map snd) |> Array.sum))
    let pmf = Map.ofArray tabulated
    let C = new Categorical((Array.map snd tabulated), gen)
    let Z = new Normal(0.0, h, gen)
    {new RV<float> with
        member this.Sample () =
            fst tabulated.[C.Sample()] + Z.Sample()
        member this.LogDensity y =
            pmf
            |> Map.map (fun x p -> exp (Z.DensityLn(y-x)))
            |> Map.toArray
            |> Array.map snd
            |> Array.sum
            |> log}


let Exponential (lambda: float) =
    let T = new Exponential(lambda, gen)
    {new RV<float> with
        member this.Sample () = T.Sample ()
        member this.LogDensity t = T.DensityLn t}

let Bernoulli (p: float) =
    let B = new Bernoulli(p, gen)
    let toBool = function
        | 0 -> false
        | _ -> true
    {new RV<bool> with
        member this.Sample () = B.Sample() |> toBool
        member this.LogDensity b =
            match b with
            | true -> log p
            | false -> log (1.0 - p)}