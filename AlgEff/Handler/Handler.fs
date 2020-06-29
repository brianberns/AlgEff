namespace AlgEff.Handler

open AlgEff.Effect

/// Effect handler base class.
/// 'ctx: Context type requirement satisfied by this handler.
/// 'res: Result type of program handled by this handler.
/// 'state: Internal state type maintained by this handler.
/// 'finish: Final state type produced by this handler.
[<AbstractClass>]
type Handler<'ctx, 'res, 'state, 'finish>() =

    /// Handler's initial state.
    abstract member Start : 'state

    /// Attempts to handle a single effect in the given state,
    /// answering the updated state and the next effect if
    /// successful.
    abstract member TryStep<'outState> :
        'state
            * Effect<Program<'ctx, 'res>>
            * HandlerCont<'ctx, 'res, 'state, 'outState>
            -> Option<'outState * 'res>

    /// Transforms the handler's final state.
    abstract member Finish : 'state -> 'finish

    /// Adapts a step function for use in an effect handler.
    member __.Adapt<'effect, 'outState when 'effect :> Effect<Program<'ctx, 'res>>>
        (step : 'state -> 'effect -> HandlerCont<'ctx, 'res, 'state, 'outState> -> ('outState * 'res)) =
        fun state (effect : Effect<_>) cont ->
            match effect with
                | :? 'effect as eff ->
                    step state eff cont |> Some
                | _ -> None

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

and HandlerCont<'ctx, 'res, 'state, 'outState> =
    'state -> Program<'ctx, 'res> -> ('outState * 'res)

/// Combines two effect handlers using the given finish.
type private CombinedHandler<'ctx, 'res, 'state1, 'finish1, 'state2, 'finish2, 'finish>
    (handler1 : Handler<'ctx, 'res, 'state1, 'finish1>,
    handler2 : Handler<'ctx, 'res, 'state2, 'finish2>,
    finish : ('finish1 * 'finish2) -> 'finish) =
    inherit Handler<'ctx, 'res, 'state1 * 'state2, 'finish>()

    /// Combined initial state.
    override __.Start = handler1.Start, handler2.Start

    /// Attempts to handle a single effect by routing it first to one handler,
    /// and then to the other.
    override __.TryStep<'outState>((state1, state2), effect, cont : HandlerCont<_, _, _, 'outState>) =
        handler1.TryStep(state1, effect, fun state1' chain ->
            cont (state1', state2) chain)
            |> Option.orElseWith (fun () ->
                handler2.TryStep(state2, effect, fun state2' chain ->
                    cont (state1, state2') chain))

    /// Combines the given handlers' final states.
    override __.Finish((state1, state2)) =
        finish (handler1.Finish(state1), handler2.Finish(state2))

module Handler =

    /// Combines two handlers using the given finish.
    let private combine handler1 handler2 finish =
        CombinedHandler(handler1, handler2, finish) :> Handler<_, _, _, _>

    /// Combines two handlers.
    let combine2 handler1 handler2 =
        combine handler1 handler2 id

    /// Combines three handlers.
    let combine3 handler1 handler2 handler3 =
        combine
            (combine2 handler1 handler2)
            handler3
            (fun ((s1, s2), s3) -> s1, s2, s3)

    /// Combines four handlers.
    let combine4 handler1 handler2 handler3 handler4 =
        combine
            (combine3 handler1 handler2 handler3)
            handler4
            (fun ((s1, s2, s3), s4) -> s1, s2, s3, s4)

    /// Combines five handlers.
    let combine5 handler1 handler2 handler3 handler4 handler5 =
        combine
            (combine4 handler1 handler2 handler3 handler4)
            handler5
            (fun ((s1, s2, s3, s4), s5) -> s1, s2, s3, s4, s5)

/// A concrete context that satisfies an effect's context
/// requirement.
[<AbstractClass>]
type ConcreteContext<'res>() = class end

/// Unit type replacement.
/// https://stackoverflow.com/questions/47909938/passing-unit-as-type-parameter-to-generic-class-in-f
type Unit = Unit
