module Fist.Main

open System
open FsCheck

let trueSigma = 3.0
let trueBeta = 3.14


type Input = float
type Response = float

let e = RV.Gaussian 0.0 trueSigma

let data :(Input*Response) list =
    [1..100]
    |> List.map (fun x -> (float x, trueBeta*(float x)+(RV.sample e)))


type Theta = {
    Beta : float
    Sigma : float
}


let beta =
    RV.Gaussian 0.0 10.0

let sigma =
    RV.Exponential (1.0/10.0)

let b = Bijection.New (fun (mu, sigma) -> {Beta=mu; Sigma=sigma}) (fun x -> (x.Beta, x.Sigma))

let prior =
    beta
    |> RV.indep sigma
    |> RV.reshape b


let lik theta (input:Input) = RV.Gaussian (theta.Beta*input) theta.Sigma


let likelihood theta =
    data
    |> List.map (fst>>(lik theta))
    |> RV.indepList


[<EntryPoint>]
let main argv = 
    printfn "Sampling..."
    let sampler = Sampler.importanceSampler prior.Sample (fun theta -> (List.map snd data) |> (RV.logDensity (likelihood theta)))
    let rawSamples = (Seq.take 1000000 sampler) |> Array.ofSeq
    let maxWeight =
        rawSamples
        |> Array.map snd
        |> Array.max
    let samples =
        rawSamples
        |> Sampler.normalize
    let E = Sampler.expectation samples
    //printf "%A\n\n" samples
    printfn "Maxweight: %f" maxWeight
    printf "%A\n\n" ((Array.map (fun (theta, w) -> (w, w-maxWeight)) rawSamples) |> List.ofSeq |> List.sortBy snd)
    printfn "True Beta: %f" trueBeta
    printfn "Posterior Mean: %f" (E (fun theta -> theta.Beta))
    printfn "Effective Samples: %f" (samples |> Array.map snd |> Sampler.effectiveSamples)
    printfn "\n\n"
    printfn "True Sigma: %f" trueSigma
    printfn "Posterior Mean: %f" (E (fun theta -> theta.Sigma))
    0 // return an integer exit code
