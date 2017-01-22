#I "bin/Debug/net45"

#r "Fist.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"
#r "Streams.dll"

open MathNet.Numerics.Distributions
open MathNet.Numerics.Random
open Nessos.Streams

open Fist


let x = [|1..20|]
Utilities.permutation x