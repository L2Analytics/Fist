module Fist.Sampler

open Nessos.Streams


type LogWeight = float

let importanceSampler (priorSample: unit -> 'theta) (logLikelihood: 'theta -> LogWeight) : Stream<'theta*LogWeight> =
    let sample x =
        let theta = priorSample ()
        (theta, logLikelihood theta)
    Stream.initInfinite sample


let effectiveSamples (weights: float array) =
    let numerator =
        weights
        |> Array.sum
        |> fun x -> x**2.0
    let denominator =
        weights
        |> Array.map (fun x -> x**2.0)
        |> Array.sum
    
    numerator/denominator



let normalize (logWeights: array<'theta*LogWeight>) :array<'theta*float> =
    (*
    let median x = 
        let a = Array.sort x
        a.[a.Length / 2]
    *)
    let normWeight =
        logWeights
        |> Array.map snd
        |> Array.max

    let unnormalized =
        logWeights
        |> Array.map (fun (theta, lw) -> (theta, exp (lw-normWeight)))
    
    let weightSum = 
        unnormalized
        |> Array.map snd
        |> Array.sum

    Array.map (fun (theta, w) -> (theta, w/weightSum)) unnormalized


let expectation (samples: array<'theta*float>) : ('theta -> float) -> float =
    fun (f: 'theta -> float) ->
            samples
            |> Array.map (fun (theta, w) -> (f theta)*w)
            |> Array.sum
