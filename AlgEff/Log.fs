﻿namespace AlgEff

type LogEff<'next>(str : string, cont : unit -> 'next) =
    inherit Effect<'next>()

    override __.Map(f) =
        LogEff(str, cont >> f) :> _

    member __.String = str

    member __.Cont = cont

type LogContext = interface end

module Log =

    let write<'ctx when 'ctx :> LogContext> str : EffectChain<'ctx, _> =
        Free (LogEff(str, Pure))

    let writef fmt = Printf.ksprintf write fmt

(* Handler *)

[<AbstractClass>]
type LogHandler<'state, 'next>() =
    inherit EffectHandler<'state, LogEff<'next>, 'next>()

type PureLogHandler<'ctx, 'res when 'ctx :> LogContext>() =
    inherit LogHandler<List<string>, EffectChain<'ctx, 'res>>()

    override __.Start = []

    override __.Step(log, logEff) =
        let state = logEff.String :: log
        let next = logEff.Cont()
        state, next

    override __.Finish(log) = List.rev log

type PureLogHandler private () =

    static member Create<'ctx, 'res when 'ctx :> LogContext>(_ : 'ctx) =
        PureLogHandler<'ctx, 'res>()
