namespace AlgEff.Handler

open AlgEff.Effect

/// Generic effect handler.
[<AbstractClass>]
type EffectHandler<'ctx, 'res, 'state, 'finish>() =

    /// Handler's initial state.
    abstract member Start : 'state

    /// Attempts to handle a single effect in the given state,
    /// answering the updated state and the next effect if
    /// successful.
    abstract member TryStep<'outState> :
        'state
            * Effect<EffectChain<'ctx, 'res>>
            * EffectHandlerCont<'ctx, 'res, 'state, 'outState>
            -> Option<'outState * 'res>

    /// Transforms the handler's final state.
    abstract member Finish : 'state -> 'finish

    /// Runs the given program.
    member this.Run(program) =

        /// Runs a single step in the program.
        let rec loop state = function
            | Free effect ->
                match this.TryStep(state, effect, loop) with
                    | Some (state', result) -> state', result
                    | None -> failwithf "Unhandled effect: %A" effect
            | Pure result ->
                state, result

        let state, result = loop this.Start program
        result, this.Finish(state)

and EffectHandlerCont<'ctx, 'res, 'state, 'outState> =
    'state -> EffectChain<'ctx, 'res> -> ('outState * 'res)

/// Combines two effect handlers.
type CombinedEffectHandler<'ctx, 'res, 'state1, 'finish1, 'state2, 'finish2>
    (handler1 : EffectHandler<'ctx, 'res, 'state1, 'finish1>,
    handler2 : EffectHandler<'ctx, 'res, 'state2, 'finish2>) =
    inherit EffectHandler<'ctx, 'res, 'state1 * 'state2, 'finish1 * 'finish2>()

    override __.Start = handler1.Start, handler2.Start

    override __.TryStep<'outState>((state1, state2), effect, cont : EffectHandlerCont<_, _, _, 'outState>) =
        let cont1 state1' chain =
            cont (state1', state2) chain
        let cont2 state2' chain =
            cont (state1, state2') chain
        handler1.TryStep(state1, effect, cont1)
            |> Option.orElseWith (fun () ->
                handler2.TryStep(state2, effect, cont2))

    override __.Finish((state1, state2)) =
        handler1.Finish(state1), handler2.Finish(state2)

/// A concrete context that satisfies an effect's context
/// requirement.
[<AbstractClass>]
type ConcreteContext<'res>() = class end
