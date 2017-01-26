module Fist.Model

open Nessos.Streams

type IDeviance<'Y> =
    abstract member Error: 'Y -> RV<'Y> -> float
    abstract member Default: 'Y array -> RV<'Y>

let performance (predictor: IPredictor<'X, 'Y>) (deviance: IDeviance<'Y>) (data: ('X*'Y) array) =
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
        let mean (x: RV<float>) =
            [|1..100|]
            |> Array.map (fun _ -> x.Sample ())
            |> Array.average
        {new IDeviance<float> with
            member this.Error y rv = (y - (mean rv))**2.0
            member this.Default ys = RV.Constant (Array.average ys)}
    
    let Bernoulli =
        let toFloat = function
                | true -> 1.0
                | false -> 0.0
        let mean (x: RV<bool>) =
            [|1..100|]
            |> Array.map (fun _ -> x.Sample () |> toFloat)
            |> Array.average
        {new IDeviance<bool> with
            member this.Error y rv =
                match y with
                | true -> log (mean rv)
                | false -> log (1.0 - (mean rv))
            member this.Default ys = ys |> Array.map toFloat |> Array.average |> RV.Bernoulli}