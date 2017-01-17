[<AutoOpen>]
module Fist.Types

type RV<'a> =
    abstract member Sample : unit -> 'a
    abstract member LogDensity : 'a -> float

type Bijection<'a, 'b> = {
    Fun : 'a -> 'b
    Inv : 'b -> 'a
}

