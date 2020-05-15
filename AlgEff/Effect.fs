namespace AlgEff

type Effect<'result> =
    | Log of string * (unit -> Effect<'result>)
    | Result of 'result

module Effect =

    let rec bind f = function
        | Log (str, cont) ->
            Log (str, fun () ->
                cont () |> bind f)
        | Result result -> f result

    let handle effect =

        let rec loop log = function
            | Log (str, cont) ->
                let log' = str :: log
                loop log' (cont ())
            | Result result -> result, log

        let result, log = loop [] effect
        result, log |> List.rev

    let log str = Log (str, fun () -> Result ())

    let logf fmt = Printf.ksprintf log fmt

type EffectBuilder() =
    member __.Return(value) = Result value
    member __.Bind(effect, f) = Effect.bind f effect

[<AutoOpen>]
module AutoOpen =
    let effect = EffectBuilder()
