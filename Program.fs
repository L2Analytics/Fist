// Learn more about F# at http://fsharp.org

open System
open FsCheck

type RV<'a> =
    abstract member Sample : unit -> 'a
    abstract member Density : 'a -> float

let (>>=) = 0

type LogWeight = float

let importanceSampler (priorSample: unit -> 'theta) (logLikelihood: 'theta -> LogWeight) : seq<'theta*LogWeight> =
    let sample x =
        let theta = priorSample ()
        (theta, logLikelihood theta)
    Seq.initInfinite sample


let makeExpectation (samples: array<'theta*float>) : ('theta -> float) -> float =
    let maxWeight = 
        samples
        |> Array.map snd
        |> Array.max
    let expectation (f: 'theta -> float) =
        samples
        |> Array.map (fun (theta, w) -> (f theta)*w)
        |> Array.sum
    expectation

type RVBuilder() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Some x

[<EntryPoint>]
let main argv = 
    printfn "Hello World!"
    0 // return an integer exit code
