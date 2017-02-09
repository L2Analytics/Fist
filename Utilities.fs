module Fist.Utilities

open MathNet.Numerics.Distributions
open MathNet.Numerics.Random

// Knuth/Fisher-Yates Shuffle
// Knuth's "Algorithm P" (according to wikipedia): https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle

let permutation (x: 'T array) =
    let y = Array.copy x
    let n = Array.length y
    for i in [|0..(n-2)|] do
        let j =  (new DiscreteUniform (i,n-1)).Sample()
        let yi = y.[i]
        y.[i] <- y.[j]
        y.[j] <- yi
    y


let equalWeight x=
    let n = Array.length x
    Array.zip x (Array.replicate n (1.0/(float n)))