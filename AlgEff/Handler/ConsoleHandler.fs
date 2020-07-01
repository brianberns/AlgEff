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
type PureConsoleHandler<'ctx, 'ret when 'ctx :> ConsoleContext and 'ctx :> ContextSatisfier<'ret>>(input, context : 'ctx) =
    inherit SimpleHandler<'ctx, 'ret, ConsoleState>()

    override __.Start = ConsoleState.create input []

    override this.TryStep(state, effect, cont) =

        let step state (consoleEff : ConsoleEffect<_>) cont =
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

        this.Adapt<_, 'stx> step state effect cont

    override __.Finish(state) =
        { state with Output = state.Output |> List.rev }

/// Actual console handler.
type ActualConsoleHandler<'ctx, 'ret when 'ctx :> ConsoleContext and 'ctx :> ContextSatisfier<'ret>>(context : 'ctx) =
    inherit SimpleHandler<'ctx, 'ret, Unit>()

    override __.Start = Unit

    override this.TryStep(state, effect, cont) =

        let step Unit (consoleEff : ConsoleEffect<_>) cont =
            match consoleEff.Case with
                | WriteLine eff ->
                    System.Console.WriteLine(eff.String)
                    let next = eff.Cont()
                    cont Unit next
                | ReadLine eff ->
                    let str = System.Console.ReadLine()
                    let next = eff.Cont(str)
                    cont Unit next

        this.Adapt<_, 'stx> step state effect cont
