namespace AlgEff

type ConsoleOp<'next> =
    inherit Op<'next>
    abstract member Effect : ConsoleEffect<'next>

and WriteLineOp<'next>(str: string, next : unit -> 'next) =
    interface ConsoleOp<'next> with
        member __.Map(f) =
            WriteLineOp(str, next >> f) :> _
        member this.Effect = WriteLine this
    member __.String = str
    member __.Next = next

and ReadLineOp<'next>(next : string -> 'next) =
    interface ConsoleOp<'next> with
        member __.Map(f) =
            ReadLineOp(next >> f) :> _
        member this.Effect = ReadLine this
    member __.Next = next

and ConsoleEffect<'next> =
    | WriteLine of WriteLineOp<'next>
    | ReadLine of ReadLineOp<'next>

type ConsoleHandler<'state, 'next> =
    inherit EffectHandler<'state, ConsoleOp<'next>, 'next>

type ConsoleHandlerCalc<'res> =
    abstract member ApplyTo<'state, 'next> : ConsoleHandler<'state, 'next> -> 'res

type ConsoleHandlerCarton =
    abstract member ApplyCalc<'res> : ConsoleHandlerCalc<'res> -> 'res

type ConsoleHandlerCartonImpl<'state, 'next>(consoleHandler : ConsoleHandler<'state, 'next>) =
    interface ConsoleHandlerCarton with
        member __.ApplyCalc<'res>(calc : ConsoleHandlerCalc<'res>) =
            calc.ApplyTo(consoleHandler)

type ConsoleHandlerCartonImpl private () =
    static member Create<'state, 'next>(consoleHandler) =
        ConsoleHandlerCartonImpl<'state, 'next>(consoleHandler) :> ConsoleHandlerCarton

type ConsoleContext = ConsoleHandlerCarton

module Console =

    let writeln<'ctx when 'ctx :> ConsoleContext> str : OpChain<'ctx, _> =
        Free (WriteLineOp(str, Pure))

    let writelnf fmt = Printf.ksprintf writeln fmt

    let readln<'ctx when 'ctx :> ConsoleContext> : OpChain<'ctx, _> =
        Free (ReadLineOp(Pure))

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
            match consoleOp.Effect with
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
