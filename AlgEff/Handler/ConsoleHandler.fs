namespace AlgEff.Handler

open AlgEff.Effect

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

/// Pure console handler.
type PureConsoleHandler<'ctx, 'res when 'ctx :> ConsoleContext and 'ctx :> ConcreteContext<'res>>(input, context : 'ctx) =
    inherit EffectHandler<'ctx, 'res, ConsoleState, ConsoleState>()

    override __.Start = ConsoleState.create input []

    override this.TryStep(state, effect, cont) =

        let step state (consoleEff : ConsoleEffect<EffectChain<'ctx, 'res>>) cont =
            match consoleEff.Case with
                | WriteLine eff ->
                    let state' =
                        { state with Output = eff.String :: state.Output }
                    let next = eff.Cont()
                    cont state' next
                | ReadLine eff ->
                    match state.Input with
                        | head :: tail ->
                            let state' =
                                let output = head :: state.Output
                                ConsoleState.create tail output
                            let next = eff.Cont(head)
                            cont state' next
                        | _ -> failwith "No more input"

        this.Adapt<_, 'outState> step state effect cont

    override __.Finish(state) =
            { state with Output = state.Output |> List.rev }

/// Actual console handler.
type ActualConsoleHandler<'ctx, 'res when 'ctx :> ConsoleContext and 'ctx :> ConcreteContext<'res>>(context : 'ctx) =
    inherit EffectHandler<'ctx, 'res, Dummy, Dummy>()

    override __.Start = Dummy

    override this.TryStep(state, effect, cont) =

        let step Dummy (consoleEff : ConsoleEffect<EffectChain<'ctx, 'res>>) cont =
            match consoleEff.Case with
                | WriteLine eff ->
                    System.Console.WriteLine(eff.String)
                    let next = eff.Cont()
                    cont Dummy next
                | ReadLine eff ->
                    let str = System.Console.ReadLine()
                    let next = eff.Cont(str)
                    cont Dummy next

        this.Adapt<_, 'outState> step state effect cont

    override __.Finish(Dummy) =
        Dummy
