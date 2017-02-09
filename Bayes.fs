module Fist.Bayes

open Nessos.Streams
open System
open System.Diagnostics

type IBayesianPredictor<'Theta, 'X, 'Y, 'Prediction> =
    inherit IPredictor<'X, 'Y, 'Prediction>
    abstract member Samples: ('Theta*float) array

type IBayesianModel<'Theta, 'X, 'Y, 'Prediction> =
    inherit IModel<'X, 'Y, 'Prediction, IBayesianPredictor<'Theta, 'X, 'Y, 'Prediction>>
    abstract member Prior: RV<'Theta>
    abstract member PriorPredictor: int -> IBayesianPredictor<'Theta, 'X, 'Y, 'Prediction>
    abstract member Likelihood: 'Theta -> 'X -> IResponse<'Y, 'Prediction>
    abstract member Sampler: ISampler<'Theta, 'X, 'Y>
and ISampler<'Theta, 'X, 'Y> =
    abstract member Sample: IBayesianModel<'Theta, 'X, 'Y, 'Prediction> -> ('X*'Y) array -> ('Theta*float) array



let equalWeight x=
    let n = Array.length x
    Array.zip x (Array.replicate n (1.0/(float n)))

let expectation (samples: array<'theta*float>) : ('theta -> float) -> float =
    fun (f: 'theta -> float) ->
            samples
            |> Array.map (fun (theta, w) -> (f theta)*w)
            |> Array.sum

let createPredictor (samples: ('Theta*float) array) (likelihood: 'Theta -> 'X -> IResponse<'Y, float>) =
    let E = expectation samples
    {new IBayesianPredictor<'Theta, 'X, 'Y, float> with
        member this.Samples = samples
        member this.Predict x = E (fun theta -> (likelihood theta x).Prediction)
    }


let createModel (prior: RV<'Theta>) (likelihood: 'Theta -> 'X -> IResponse<'Y, float>) (sampler: ISampler<'Theta, 'X, 'Y>) =
    let priorPredictor n =
        let samples =
            Array.init n (fun _ -> prior.Sample())
            |> equalWeight
        {new IBayesianPredictor<'Theta, 'X, 'Y, float> with
            member this.Samples = samples
            member this.Predict x =
                let E = expectation samples
                E (fun theta -> (likelihood theta x).Prediction)}
    
    {new IBayesianModel<'Theta, 'X, 'Y, float> with
        member this.Prior = prior
        member this.PriorPredictor n = priorPredictor n
        member this.Likelihood t x = likelihood t x
        member this.Sampler = sampler
        member this.Fit data = createPredictor (sampler.Sample this data) likelihood}




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

    let samples (n: int) :Stream<'Theta*float> -> ('Theta*float) array =
        (Stream.take n) >> Stream.toArray


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
                    |> Array.map (fun (x, y) ->(model.Likelihood z x).RV.LogDensity y)
                    |> Array.sum
                
                let sample x =
                    let theta = model.Prior.Sample ()
                    (theta, lik theta)
                
                Stream.initInfinite sample
                |> stopping
                |> normalize}