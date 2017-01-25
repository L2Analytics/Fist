module Fist.Bayes

open Nessos.Streams
open System
open System.Diagnostics


type IBayesianPredictor<'Theta, 'X, 'Y> =
    inherit IPredictor<'X, 'Y>
    abstract member Samples : ('Theta*float) array

type IBayesianModel<'Theta, 'X, 'Y> =
    inherit IModel<'X, 'Y>
    abstract member Prior : RV<'Theta>
    abstract member Likelihood : 'Theta -> 'X -> RV<'Y>

type ISampler = 
    abstract Generate: IBayesianModel<'Theta, 'X, 'Y> -> Stream<'Theta*float>
    abstract Stopping: Stream<'Theta*float> -> ('Theta*float) array

let expectation (samples: array<'theta*float>) : ('theta -> float) -> float =
    fun (f: 'theta -> float) ->
            samples
            |> Array.map (fun (theta, w) -> (f theta)*w)
            |> Array.sum

let sample (s: ISampler) (m: IBayesianModel<'Theta, 'X, 'Y>) = s.Generate m |> s.Stopping




module Stopping =
    let seconds (runtime: int) :Stream<'Theta*float> -> ('Theta*float) array =
        let timer = new Stopwatch()

        let f s =
            //is this necessary?
            Stream.take 1 s |> ignore
            timer.Start ()
            s
            |> Stream.takeWhile (fun _ -> timer.ElapsedMilliseconds < int64 (runtime*1000))
            |> Stream.toArray
        f


module IS =

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
        let maxWeight =
            logWeights
            |> Array.map snd
            |> Array.max

        let unnormalized =
            logWeights
            |> Array.map (fun (theta, lw) -> (theta, exp (lw-maxWeight)))
        
        let weightSum = 
            unnormalized
            |> Array.map snd
            |> Array.sum

        Array.map (fun (theta, w) -> (theta, w/weightSum)) unnormalized

