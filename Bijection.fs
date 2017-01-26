module Fist.Bijection


let New (f: 'a -> 'b) (fInv: 'b -> 'a) =
    {
        Fun = f
        Inv = fInv
    }

let compose (g: Bijection<'b, 'c>) (f: Bijection<'a, 'b>) :Bijection<'a, 'c> =
    {
        Fun = f.Fun >> g.Fun
        Inv = g.Inv >> f.Inv
    }