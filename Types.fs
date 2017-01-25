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


type IBayesianPredictor<'Theta, 'X, 'Y> =
    inherit IPredictor<'X, 'Y>
    abstract member Samples : ('Theta*float) array

type IBayesianModel<'Theta, 'X, 'Y> =
    inherit IModel<'X, 'Y>
    abstract member Prior : RV<'Theta>
    abstract member Likelihood : 'Theta -> 'X -> RV<'Y>

type ISampler = 
    abstract Sample: IBayesianModel<'Theta, 'X, 'Y> -> ('Theta*float) array