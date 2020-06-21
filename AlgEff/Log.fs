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

module LogHandler =

    let createPure<'ctx, 'res when 'ctx :> LogContext and 'ctx :> ConcreteContext<'res>> (_ : 'ctx) =

        let step (log, (logEff : LogEff<EffectChain<'ctx, 'res>>)) =
            let state = logEff.String :: log
            let next = logEff.Cont()
            state, next

        EffectHandler.create [] step List.rev
