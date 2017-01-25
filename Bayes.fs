module Fist.Bayes

open Nessos.Streams
open System
open System.Diagnostics



type BayesianPredictor<'Theta, 'X, 'Y> = {
    Likelihood : 'Theta -> 'X -> RV<'Y>
    Posterior : RV<'Theta>
    Samples : ('Theta*float) array
} with
    interface IPredictor<'X, 'Y> with
        member this.Predict x =
            {new RV<'Y> with
                member rv.Sample () =
                    x
                    |> this.Likelihood (this.Posterior.Sample())
                    |> RV.sample
                //This is definitely something that could be abstracted, but at the cost of efficiency...perhaps reconsider
                member rv.LogDensity y =
                    this.Samples
                    |> Array.map (fun (theta, w) ->
                                    (this.Likelihood theta x)
                                    |> (fun r -> RV.logDensity r y)
                                    |> (fun x -> x*w))
                    |> Array.sum}

//The type constraint is needed only because of the implementation of RV.Discrete
type BayesianModel<'Theta, 'X, 'Y when 'Theta: comparison> = {
    Prior : RV<'Theta>
    Likelihood : 'Theta -> 'X -> RV<'Y>
    Sampler : ISampler
} with
    interface IModel<'X, 'Y> with
        member this.Fit data =
            let samples = this.Sampler.Generate this data |> this.Sampler.Stopping
            {
                BayesianPredictor.Likelihood = this.Likelihood
                Posterior = RV.Discrete samples
                Samples = samples
            } :> IPredictor<'X, 'Y>
and ISampler = 
    abstract Generate: BayesianModel<'Theta, 'X, 'Y> -> ('X * 'Y) array -> Stream<'Theta*float>
    abstract Stopping: Stream<'Theta*float> -> ('Theta*float) array

let expectation (samples: array<'theta*float>) : ('theta -> float) -> float =
    fun (f: 'theta -> float) ->
            samples
            |> Array.map (fun (theta, w) -> (f theta)*w)
            |> Array.sum

let sample (s: ISampler) (data: ('X * 'Y) array) (m: BayesianModel<'Theta, 'X, 'Y>) = s.Generate m data |> s.Stopping




module Stopping =
    let seconds (runtime: int) :Stream<'Theta*float> -> ('Theta*float) array =
        let timer = new Stopwatch()

        let f s =
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

