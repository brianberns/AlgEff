namespace AlgEff

type LogEff<'next>(str : string, cont : unit -> 'next) =
    interface Effect<'next> with
        member __.Map(f) =
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
    inherit EffectHandler<'state, LogEff<'next>, 'next>

type LogHandlerOp<'res> =
    abstract member ApplyTo<'state, 'next> : LogHandler<'state, 'next> -> 'res

type LogHandlerCarton =
    inherit LogContext
    abstract member ApplyOp<'res> : LogHandlerOp<'res> -> 'res

type LogHandlerCartonImpl<'state, 'next>(logHandler : LogHandler<'state, 'next>) =
    interface LogHandlerCarton with
        member __.ApplyOp<'res>(op : LogHandlerOp<'res>) =
            op.ApplyTo(logHandler)

type LogHandlerCartonImpl private () =
    static member Create<'state, 'next>(logHandler) =
        LogHandlerCartonImpl<'state, 'next>(logHandler) :> LogHandlerCarton

type PureLogHandler<'ctx, 'res when 'ctx :> LogContext>() =

    interface LogHandler<List<string>, EffectChain<'ctx, 'res>> with

        member __.Start = []

        member __.Step(log, logEff) =
            let state = logEff.String :: log
            let next = logEff.Cont()
            state, next

        member __.Finish(log) = List.rev log

type PureLogHandler private () =

    static member CreateCarton<'ctx, 'res when 'ctx :> LogContext>(_ : 'ctx) =
        PureLogHandler<'ctx, 'res>()
            |> LogHandlerCartonImpl.Create
