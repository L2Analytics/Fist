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
    abstract member Sampler: ISampler
and ISampler =
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

let createModel (prior: RV<'Theta>) (likelihood: 'Theta -> 'X -> RV<'Y>) (sampler: ISampler) =
    {new IBayesianModel<'Theta, 'X, 'Y> with
        member this.Prior = prior
        member this.Likelihood t x = likelihood t x
        member this.Sampler = sampler
        member this.Fit data = createPredictor (sampler.Sample this data) likelihood}

(*
type BayesianPredictor<'Theta, 'X, 'Y> = {
    Likelihood : 'Theta -> 'X -> RV<'Y>
    Samples : ('Theta*float) array
} with
    interface IPredictor<'X, 'Y> with
        member this.Predict x =
            {new RV<'Y> with
                member rv.Sample () =
                    x
                    |> this.Likelihood (RV.sampleWeighted this.Samples)
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
type BayesianModel<'Theta, 'X, 'Y> = {
    Prior : RV<'Theta>
    Likelihood : 'Theta -> 'X -> RV<'Y>
    Sampler : ISampler<'Theta, 'X, 'Y>
} with
    interface IModel<'X, 'Y> with
        member this.Fit data =
            let samples = this.Sampler.Generate this data |> this.Sampler.Stopping
            {
                BayesianPredictor.Likelihood = this.Likelihood
                Samples = samples
            } :> IPredictor<'X, 'Y>
and ISampler<'Theta, 'X, 'Y> = 
    abstract Generate: BayesianModel<'Theta, 'X, 'Y> -> ('X * 'Y) array -> Stream<'Theta*float>
    abstract Stopping: Stream<'Theta*float> -> ('Theta*float) array
*)
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

(*
    let New (stopping: Stream<'Theta*float> -> ('Theta*float) array) =
        {new ISampler with
            member this.Generate model data =
                let lik z =
                    data
                    |> Array.map (fun (x, y) -> RV.logDensity (model.Likelihood z x) y)
                    |> Array.sum
                
                let sample (x:int) =
                    let theta = model.Prior.Sample ()
                    (theta, lik theta)
                
                Stream.initInfinite sample
            member this.Stopping s = stopping s}
*)

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

