module Fist.Bayes

open Nessos.Streams
open System
open System.Diagnostics

type IBayesianPredictor<'Theta, 'X, 'Y> =
    inherit IPredictor<'X, 'Y>
    abstract member Samples: ('Theta*float) array

type IBayesianModel<'Theta, 'X, 'Y> =
    inherit IModel<'X, 'Y, IBayesianPredictor<'Theta, 'X, 'Y>>
    abstract member Prior: RV<'Theta>
    abstract member Likelihood: 'Theta -> 'X -> RV<'Y>
    abstract member Sampler: ISampler<'Theta, 'X, 'Y>
and ISampler<'Theta, 'X, 'Y> =
    abstract member Sample: IBayesianModel<'Theta, 'X, 'Y> -> ('X*'Y) array -> ('Theta*float) array

let createPredictor (samples: ('Theta*float) array) (likelihood: 'Theta -> 'X -> RV<'Y>) =
    {new IBayesianPredictor<'Theta, 'X, 'Y> with
        member this.Samples = samples
        member this.Predict x =
            {new RV<'Y> with
                member rv.Sample () =
                    x
                    |> likelihood (RV.sampleWeighted this.Samples)
                    |> RV.sample
                //This is definitely something that could be abstracted, but at the cost of efficiency...perhaps reconsider
                member rv.LogDensity y =
                    this.Samples
                    |> Array.map (fun (theta, w) ->
                                    (likelihood theta x)
                                    |> (fun r -> RV.logDensity r y)
                                    |> (fun x -> x*w))
                    |> Array.sum}}

let createModel (prior: RV<'Theta>) (likelihood: 'Theta -> 'X -> RV<'Y>) (sampler: ISampler<'Theta, 'X, 'Y>) =
    {new IBayesianModel<'Theta, 'X, 'Y> with
        member this.Prior = prior
        member this.Likelihood t x = likelihood t x
        member this.Sampler = sampler
        member this.Fit data = createPredictor (sampler.Sample this data) likelihood}


let expectation (samples: array<'theta*float>) : ('theta -> float) -> float =
    fun (f: 'theta -> float) ->
            samples
            |> Array.map (fun (theta, w) -> (f theta)*w)
            |> Array.sum

module Stopping =
    let seconds (runtime: int) :Stream<'Theta*float> -> ('Theta*float) array =
        let timer = new Stopwatch()

        let f s =
            timer.Start ()
            s
            |> Stream.takeWhile (fun _ -> timer.ElapsedMilliseconds < int64 (runtime*1000))
            |> Stream.toArray
            |> (fun s -> timer.Reset(); s)
        f


module IS =

    type LogWeight = float

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


    let create stopping =
        {new ISampler<'Theta, 'X, 'Y> with
            member this.Sample model data =
                let lik z =
                    data
                    |> Array.map (fun (x, y) -> RV.logDensity (model.Likelihood z x) y)
                    |> Array.sum
                
                let sample x =
                    let theta = model.Prior.Sample ()
                    (theta, lik theta)
                
                Stream.initInfinite sample
                |> stopping
                |> normalize}