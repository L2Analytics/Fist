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

type IDeviance<'Y, 'Prediction> =
    abstract member Error: 'Y -> 'Prediction -> float
    abstract member Default: 'Y array -> 'Prediction

type IResponse<'Y, 'Prediction> =
    abstract member RV: RV<'Y>
    abstract member Prediction: 'Prediction
    abstract member Deviance: IDeviance<'Y, 'Prediction>

type IPredictor<'X, 'Y, 'Prediction> =
    abstract member Predict: 'X -> 'Prediction

type IModel<'X, 'Y, 'Prediction, 'Predictor when 'Predictor :> IPredictor<'X, 'Y, 'Prediction>> =
    abstract member Fit: ('X*'Y) array -> 'Predictor
