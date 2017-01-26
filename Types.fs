[<AutoOpen>]
module Fist.Types

open Nessos.Streams

type Bijection<'a, 'b> = {
    Fun : 'a -> 'b
    Inv : 'b -> 'a
}

type RV<'a> =
    abstract member Sample : unit -> 'a
    abstract member LogDensity : 'a -> float

type IPredictor<'X, 'Y> =
    abstract Predict: 'X -> RV<'Y>

type IModel<'X, 'Y, 'P when 'P :> IPredictor<'X, 'Y>> =
    abstract Fit: ('X*'Y) array -> 'P
