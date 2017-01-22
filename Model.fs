module Fist.Model


let create (prior: RV<'Theta>) (likelihood: 'Theta -> 'X -> RV<'Y>) =
    {
        Prior = prior
        Likelihood = likelihood
    }


let fit (model: Model<'Theta, 'X, 'Y>

module Performance =
    let gaussian (