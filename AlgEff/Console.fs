namespace AlgEff

type ConsoleEff<'next> =
    inherit Effect<'next>
    abstract member Case : ConsoleEffSum<'next>

and WriteLineEff<'next>(str : string, cont : unit -> 'next) =
    interface ConsoleEff<'next> with
        member __.Map(f) =
            WriteLineEff(str, cont >> f) :> _
        member this.Case = WriteLine this
    member __.String = str
    member __.Cont = cont

and ReadLineEff<'next>(cont : string -> 'next) =
    interface ConsoleEff<'next> with
        member __.Map(f) =
            ReadLineEff(cont >> f) :> _
        member this.Case = ReadLine this
    member __.Cont = cont

/// Sum type for console effects.
and ConsoleEffSum<'next> =
    | WriteLine of WriteLineEff<'next>
    | ReadLine of ReadLineEff<'next>

type ConsoleContext = interface end

module Console =

    let writeln<'ctx when 'ctx :> ConsoleContext> str : EffectChain<'ctx, _> =
        Free (WriteLineEff(str, Pure))

    let writelnf fmt = Printf.ksprintf writeln fmt

    let readln<'ctx when 'ctx :> ConsoleContext> : EffectChain<'ctx, _> =
        Free (ReadLineEff(Pure))

(* Handler *)

type ConsoleHandler<'state, 'next> =
    inherit EffectHandler<'state, ConsoleEff<'next>, 'next>

type ConsoleState =
    {
        Input : List<string>
        Output : List<string>
    }

module ConsoleState =

    let create input output =
        {
            Input = input
            Output = output
        }

type PureConsoleHandler<'ctx, 'res when 'ctx :> ConsoleContext>(input) =

    interface ConsoleHandler<ConsoleState, EffectChain<'ctx, 'res>> with

        member __.Start = ConsoleState.create input []

        member __.Step(state, consoleEff) =
            match consoleEff.Case with
                | WriteLine eff ->
                    let state' =
                        { state with Output = eff.String :: state.Output }
                    let next = eff.Cont()
                    state', next
                | ReadLine eff ->
                    match state.Input with
                        | head :: tail ->
                            let state' =
                                let output = head :: state.Output
                                ConsoleState.create tail output
                            let next = eff.Cont(head)
                            state', next
                        | _ -> failwith "No more input"

        member __.Finish(state) =
            { state with Output = state.Output |> List.rev }

type PureConsoleHandler private () =

    static member Create<'ctx, 'res when 'ctx :> ConsoleContext>(_ : 'ctx, input) =
        PureConsoleHandler<'ctx, 'res>(input)
