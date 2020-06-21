namespace AlgEff

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

type LogHandler<'state, 'next> =
    EffectHandler<'state, LogEff<'next>, 'next, 'state>

module LogHandler =

    let pureHandler<'ctx, 'res when 'ctx :> LogContext> : LogHandler<_, _> =

        let step (log, (logEff : LogEff<EffectChain<'ctx, 'res>>)) =
            let state = logEff.String :: log
            let next = logEff.Cont()
            state, next

        EffectHandler.create [] step List.rev

    let createPureCtx<'ctx, 'res when 'ctx :> LogContext> (_ : 'ctx) =
        pureHandler<'ctx, 'res>
