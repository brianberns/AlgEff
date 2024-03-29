﻿namespace AlgEff.Handler

open AlgEff.Effect

/// Effect handler base class.
/// 'ctx: Context type requirement satisfied by this handler.
/// 'ret: Return type of program handled by this handler.
/// 'st:  Internal state type maintained by this handler.
/// 'fin: Final state type produced by this handler.
[<AbstractClass>]
type Handler<'ctx, 'ret, 'st, 'fin>() =

    /// Handler's initial state.
    abstract member Start : 'st

    /// Attempts to handle a single effect in a progam.
    /// 'stx: State answered by the continuation, which may be
    ///       different from the state managed by this handler.
    abstract member TryStep<'stx> :
        'st                                        // state before handling current effect
            * Effect<'ctx, 'ret>                   // effect to be handled
            * HandlerCont<'ctx, 'ret, 'st, 'stx>   // continuation that will handle the remainder of the program
            -> Option<List<'ret * 'stx>>           // "Some" indicates the effect was handled

    /// Transforms the handler's final state.
    abstract member Finish : 'st -> 'fin

    /// Runs the given program, producing a list of results.
    member this.RunMany(program) =

        /// Runs a single step in the program.
        let rec loop state = function
            | Free effect ->
                this.TryStep(state, effect, loop)
                    |> Option.defaultWith (fun () ->
                        failwithf "Unhandled effect: %A" effect)
            | Pure ret ->
                [ ret, state ]

        loop this.Start program
            |> List.map (fun (ret, state) ->
                ret, this.Finish(state))

    /// Runs the given program, producing a single result.
    member this.Run(program) =
        program |> this.RunMany |> List.exactlyOne

/// Continuation that handles the remainder of a program.
and HandlerCont<'ctx, 'ret, 'st, 'stx> =
    'st                          // state after handling current effect
        -> Program<'ctx, 'ret>   // remainder of the program to handle
        -> List<'ret * 'stx>     // output of handling the program

/// Handler whose final state type is the same as its internal state type.
[<AbstractClass>]
type SimpleHandler<'ctx, 'ret, 'st>() =
    inherit Handler<'ctx, 'ret, 'st, 'st>()

    /// No-op final transformation.
    default _.Finish(state) = state

/// Combines two effect handlers using the given finish.
type private CombinedHandler<'ctx, 'ret, 'st1, 'fin1, 'st2, 'fin2, 'fin>
    (handler1 : Handler<'ctx, 'ret, 'st1, 'fin1>,
    handler2 : Handler<'ctx, 'ret, 'st2, 'fin2>,
    finish : ('fin1 * 'fin2) -> 'fin) =
    inherit Handler<'ctx, 'ret, 'st1 * 'st2, 'fin>()

    /// Combined initial state.
    override _.Start = handler1.Start, handler2.Start

    /// Attempts to handle a single effect by routing it first to one handler,
    /// and then to the other.
    override _.TryStep<'stx>((state1, state2), effect, cont : HandlerCont<_, _, _, 'stx>) =
        handler1.TryStep(state1, effect, fun state1' program ->
            cont (state1', state2) program)
            |> Option.orElseWith (fun () ->
                handler2.TryStep(state2, effect, fun state2' program ->
                    cont (state1, state2') program))

    /// Combines the given handlers' final states.
    override _.Finish((state1, state2)) =
        finish (handler1.Finish(state1), handler2.Finish(state2))

module Handler =

    /// Adapts a step function for use in an effect handler.
    let tryStep<'eff, 'next, 'ret when 'eff :> Effect<'next>>
        (effect : Effect<'next>)
        (step : 'eff -> 'ret) =
            match effect with
                | :? 'eff as eff -> step eff |> Some
                | _ -> None

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

/// Base type for concrete classes that satisfy an effect type's context
/// requirement.
[<AbstractClass>]
type Environment<'ret>() = class end

/// Unit type replacement.
/// https://stackoverflow.com/questions/47909938/passing-unit-as-type-parameter-to-generic-class-in-f
type Unit = Unit
