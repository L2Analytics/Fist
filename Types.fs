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


type IAdult =
    abstract member DoAdultThing: unit -> string

type IChild<'T when 'T :> IAdult>=
    abstract member GrowUp: unit -> 'T

type IGrownDog =
    inherit IAdult
    abstract member Bark: unit -> string

type IPuppy =
    inherit IChild<IGrownDog>
    abstract member DoPuppyThing: unit -> string