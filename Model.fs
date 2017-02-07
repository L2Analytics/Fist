module Fist.Model

open Nessos.Streams



let performance (predictor: IPredictor<'X, 'Y, 'Prediction>) (deviance: IDeviance<'Y, 'Prediction>) (data: ('X*'Y) array) =
    let y0 =
        data
        |> Array.map snd
        |> deviance.Default
    data
    |> Stream.ofArray
    |> Stream.map (fun (x, y) -> (y, predictor.Predict x))
    |> Stream.map (fun (y, yhat) -> (deviance.Error y y0, deviance.Error y yhat))
    |> Stream.fold (fun (total0, total) (e0, e) -> (total0+e0, total+e)) (0.0, 0.0)
    |> fun (t0, t) -> 1.0-t/t0


module Deviance =
    let Gaussian =
        {new IDeviance<float, float> with
            member this.Error y yhat = (y - yhat)**2.0
            member this.Default ys = (Array.average ys)}
    
//Currently just hard-code a threshold of .05--all predictions are truncated to [0.05, 0.95]    
    let Bernoulli =
        let toFloat = function
                | true -> 1.0
                | false -> 0.0
        let threshold x = (min x 0.95) |> (max 0.05)    
        {new IDeviance<bool, float> with
            member this.Error y yhat =
                let y0 = threshold yhat
                match y with
                | true -> log y0
                | false -> log (1.0 - y0)
            member this.Default ys = ys |> Array.map toFloat |> Array.average}

module Response =
    let Bernoulli p =
        {new IResponse<bool, float> with
            member this.RV = RV.Bernoulli p
            member this.Prediction = p
            member this.Deviance = Deviance.Bernoulli}