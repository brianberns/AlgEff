namespace AlgEff

type ConsoleEff<'next> =
    inherit Effect<'next>
    abstract member Case : ConsoleEffSum<'next>

and WriteLineEff<'next>(str: string, next : unit -> 'next) =
    interface ConsoleEff<'next> with
        member __.Map(f) =
            WriteLineEff(str, next >> f) :> _
        member this.Case = WriteLine this
    member __.String = str
    member __.Next = next

and ReadLineEff<'next>(next : string -> 'next) =
    interface ConsoleEff<'next> with
        member __.Map(f) =
            ReadLineEff(next >> f) :> _
        member this.Case = ReadLine this
    member __.Next = next

/// Sum type for console effects.
and ConsoleEffSum<'next> =
    | WriteLine of WriteLineEff<'next>
    | ReadLine of ReadLineEff<'next>

(* Handler *)

type ConsoleHandler<'state, 'next> =
    inherit EffectHandler<'state, ConsoleEff<'next>, 'next>

type ConsoleHandlerOp<'res> =
    abstract member ApplyTo<'state, 'next> : ConsoleHandler<'state, 'next> -> 'res

type ConsoleHandlerCarton =
    abstract member ApplyOp<'res> : ConsoleHandlerOp<'res> -> 'res

type ConsoleHandlerCartonImpl<'state, 'next>(consoleHandler : ConsoleHandler<'state, 'next>) =
    interface ConsoleHandlerCarton with
        member __.ApplyOp<'res>(op : ConsoleHandlerOp<'res>) =
            op.ApplyTo(consoleHandler)

type ConsoleHandlerCartonImpl private () =
    static member Create<'state, 'next>(consoleHandler) =
        ConsoleHandlerCartonImpl<'state, 'next>(consoleHandler) :> ConsoleHandlerCarton

type ConsoleContext = ConsoleHandlerCarton

module Console =

    let writeln<'ctx when 'ctx :> ConsoleContext> str : EffectChain<'ctx, _> =
        Free (WriteLineEff(str, Pure))

    let writelnf fmt = Printf.ksprintf writeln fmt

    let readln<'ctx when 'ctx :> ConsoleContext> : EffectChain<'ctx, _> =
        Free (ReadLineEff(Pure))

type ConsoleState =
    {
        Input : List<string>
        Output : List<string>
    }

module ConsoleState =

    let create input =
        {
            Input = input
            Output = []
        }

type ConsoleHandler<'next>(input) =

    interface ConsoleHandler<ConsoleState, 'next> with

        member __.Start = ConsoleState.create input

        member __.Step(state, consoleOp) =
            match consoleOp.Case with
                | WriteLine op ->
                    let state' =
                        { state with Output = op.String :: state.Output }
                    let next = op.Next ()
                    state', next
                | ReadLine op ->
                    match state.Input with
                        | head :: tail ->
                            let state' =
                                { state with Input = tail }
                            let next = op.Next head
                            state', next
                        | _ -> failwith "No more input"

        member __.Finish(state) =
            { state with Output = state.Output |> List.rev }
