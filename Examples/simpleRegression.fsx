#I "../bin/Debug/net45/"

#r "Fist.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"
#r "Streams.dll"

open System
open MathNet.Numerics.Distributions
open MathNet.Numerics.Random
open Nessos.Streams

open Fist
open Fist.Bayes

let trueSigma = 3.0
let trueBeta = 3.14


type Input = float
type Response = float

let e = RV.Gaussian 0.0 trueSigma

let data :(Input*Response) array =
    [|1..100|]
    |> Array.map (fun x -> (float x, trueBeta*(float x)+(RV.sample e)))


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


let model = Bayes.createModel prior lik (Bayes.IS.create (Bayes.Stopping.seconds 10))



printfn "Sampling..."
let pred = model.Fit data
let E = Bayes.expectation pred.Samples

//printf "%A\n\n" samples
printfn "True Beta: %f" trueBeta
printfn "Posterior Mean: %f" (E (fun theta -> theta.Beta))
printfn "Posterior Var: %f" ((E (fun theta -> theta.Beta**2.0)) - (E (fun theta -> theta.Beta))**2.0)
printfn "Effective Samples: %f" (pred.Samples |> Array.map snd |> Bayes.IS.effectiveSamples)
printfn "\n\n"
printfn "True Sigma: %f" trueSigma
printfn "Posterior Mean: %f" (E (fun theta -> theta.Sigma))

