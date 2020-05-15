namespace AlgEff

type LogEffect<'result> =
    | LogMsg of string * (unit -> LogEffect<'result>)
    | Done of 'result

module LogEffect =

    let rec bind f = function
        | LogMsg (str, cont) ->
            LogMsg (str, fun () ->
                cont () |> bind f)
        | Done result -> f result

    let pureHandler logEffect =

        let rec loop log logEffect =
            match logEffect with
                | LogMsg (str, cont) ->
                    let log' = str :: log
                    loop log' (cont ())
                | Done result -> result, log

        loop [] logEffect

    let log str = LogMsg (str, fun () -> Done ())

    let logf fmt = Printf.ksprintf log fmt

type Builder() =
    member __.Return(value) = Done value
    member __.Bind(effect, f) = LogEffect.bind f effect

[<AutoOpen>]
module AutoOpen =
    let log = Builder()
