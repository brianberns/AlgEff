namespace AlgEff

type LogOp<'next>(str: string, next : unit -> 'next) =
    interface Op<'next> with
        member __.Map(f) =
            LogOp(str, next >> f) :> _
    member __.String = str
    member __.Next = next

type LogHandler = interface end

module Log =

    let write<'ctx when 'ctx :> LogHandler> str : OpChain<'ctx, _> =
        Free (LogOp(str, Pure))

    let writef fmt = Printf.ksprintf write fmt

module LogHandler =

    let handle<'next> =
        {
            Init = []
            Step =
                fun acc (effect : LogOp<'next>) ->
                    let state = effect.String :: acc
                    let next = effect.Next ()
                    state, next
            Final = List.rev
        }
