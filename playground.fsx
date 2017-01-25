#I "bin/Debug/net45"

#r "Fist.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"
#r "Streams.dll"

open System
open MathNet.Numerics.Distributions
open MathNet.Numerics.Random
open Nessos.Streams

open Fist


let x = [|1..20|]
Utilities.permutation x


// Put in some timing to see how much the sampling is slowed down by checking the timer every sample
let stop = Bayes.Stopping.seconds 1
System.Threading.Thread.Sleep 1000
let inf =
    Seq.initInfinite (fun i -> (i, float(i)))
    |> Stream.ofSeq
    |> stop

