[<AutoOpen>]
module Fist.Types

open Nessos.Streams

type RV<'a> =
    abstract member Sample : unit -> 'a
    abstract member LogDensity : 'a -> float

type Bijection<'a, 'b> = {
    Fun : 'a -> 'b
    Inv : 'b -> 'a
}


type IPredictor<'X, 'Y> =
    abstract Predict: 'X -> RV<'Y>

type IModel<'X, 'Y> =
    abstract Fit: 'X*'Y array -> IPredictor<'X, 'Y>


type BayesianModel<'Theta, 'X, 'Y> = {
    Prior : RV<'Theta>
    Likelihood : 'Theta -> 'X -> RV<'Y>
}

type ISampler = 
    abstract Sample: BayesianModel<'Theta, 'X, 'Y> -> IModel<'X, 'Y>