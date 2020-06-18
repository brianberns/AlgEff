namespace AlgEff

type LogOp<'a>(str: string, next : unit -> 'a) =
    interface Op<'a> with
        member __.Map(f) =
            Log(str, next >> f) :> _
    member __.String = str
    member __.Next = next

type LogHandler =
    abstract member Handle<'a> : Log<'a> -> unit

module Log =

    let write<'ctx when 'ctx :> LogHandler> str : OpChain<'ctx, _> =
        Free (Log(str, Pure))

    let writef fmt = Printf.ksprintf write fmt

    let handle<'a> =
        {|
            Init = List.empty<string>
            Step =
                fun acc (effect : Log<'a>) ->
                    let state = effect.String :: acc
                    let next = effect.Next ()
                    state, next
            Final = List.rev
        |}
