namespace AlgEff.Handler

open AlgEff.Effect

type ConsoleHandler<'next, 'state> =
    EffectHandler<ConsoleEff<'next>, 'next, 'state, 'state>

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

module ConsoleHandler =

    let createPure<'ctx, 'res when 'ctx :> ConsoleContext and 'ctx :> ConcreteContext<'res>>
        input (_ : 'ctx) =

        let start = ConsoleState.create input []

        let step (state, (consoleEff : ConsoleEff<EffectChain<'ctx, 'res>>)) =
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

        let finish state =
            { state with Output = state.Output |> List.rev }

        EffectHandler.create start step finish
