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

    override __.TryStep(state, effect, cont) =
        match effect with
            | :? ConsoleEffect<EffectChain<'ctx, 'res>> as consoleEff ->
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
                    |> Some
            | _ -> None

    override __.Finish(state) =
            { state with Output = state.Output |> List.rev }

(*
    /// Actual console handler.
    let createActual<'ctx, 'res when 'ctx :> ConsoleContext and 'ctx :> ConcreteContext<'res>>
        (_ : 'ctx) =

        let step ((), (consoleEff : ConsoleEffect<EffectChain<'ctx, 'res>>)) =
            let next =
                match consoleEff.Case with
                    | WriteLine eff ->
                        System.Console.WriteLine(eff.String)
                        eff.Cont()
                    | ReadLine eff ->
                        let str = System.Console.ReadLine()
                        eff.Cont(str)
            (), next

        EffectHandler.adapt () step id
*)
