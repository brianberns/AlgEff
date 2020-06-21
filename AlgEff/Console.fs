namespace AlgEff

/// Base console effect type.
[<AbstractClass>]
type ConsoleEff<'next>() =
    inherit Effect<'next>()

    abstract member Case : ConsoleEffSum<'next>

and WriteLineEff<'next>(str : string, cont : unit -> 'next) =
    inherit ConsoleEff<'next>()

    override __.Map(f) =
        WriteLineEff(str, cont >> f) :> _

    override this.Case = WriteLine this

    member __.String = str

    member __.Cont = cont

and ReadLineEff<'next>(cont : string -> 'next) =
    inherit ConsoleEff<'next>()

    override __.Map(f) =
        ReadLineEff(cont >> f) :> _

    override this.Case = ReadLine this

    member __.Cont = cont

/// Sum type for console effects.
and ConsoleEffSum<'next> =
    | WriteLine of WriteLineEff<'next>
    | ReadLine of ReadLineEff<'next>

/// Console context.
type ConsoleContext = interface end

module Console =

    let writeln<'ctx when 'ctx :> ConsoleContext> str : EffectChain<'ctx, _> =
        Free (WriteLineEff(str, Pure))

    let writelnf fmt = Printf.ksprintf writeln fmt

    let readln<'ctx when 'ctx :> ConsoleContext> : EffectChain<'ctx, _> =
        Free (ReadLineEff(Pure))

(* Handler *)

[<AbstractClass>]
type ConsoleHandler<'state, 'next>() =
    inherit EffectHandler<'state, ConsoleEff<'next>, 'next>()

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
    inherit ConsoleHandler<ConsoleState, EffectChain<'ctx, 'res>>()

    override __.Start = ConsoleState.create input []

    override __.Step(state, consoleEff) =
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

    override __.Finish(state) =
        { state with Output = state.Output |> List.rev }

type PureConsoleHandler private () =

    static member Create<'ctx, 'res when 'ctx :> ConsoleContext>(_ : 'ctx, input) =
        PureConsoleHandler<'ctx, 'res>(input)
