[<AutoOpen>]
module Fist.Types

type RV<'a> =
    abstract member Sample : unit -> 'a
    abstract member LogDensity : 'a -> float

type Bijection<'a, 'b> = {
    Fun : 'a -> 'b
    Inv : 'b -> 'a
}


type Model<'X, 'Y> = 'X*'Y array -> ('X -> RV<'Y>) 




type IBayes =
    abstract Prior: unit -> RV<'Theta>
    abstract Likelihood: 'Theta -> 'X -> RV<'Y>

type BayesianModel<'Theta, 'X, 'Y> = {
    Prior : RV<'Theta>
    Likelihood : 'Theta -> 'X -> RV<'Y>
}

type ISampler = 
    abstract Sample: BayesianModel<'Theta, 'X, 'Y> -> Model<'X, 'Y>