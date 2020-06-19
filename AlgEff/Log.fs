﻿namespace AlgEff

type LogEff<'next>(str: string, next : unit -> 'next) =
    interface Effect<'next> with
        member __.Map(f) =
            LogEff(str, next >> f) :> _
    member __.String = str
    member __.Next = next

type LogHandler = interface end

module Log =

    let write<'ctx when 'ctx :> LogHandler> str : EffectChain<'ctx, _> =
        Free (LogEff(str, Pure))

    let writef fmt = Printf.ksprintf write fmt

(*
module LogHandler =

    let handle<'next> =
        {
            Init = []
            Step =
                fun acc (effect : LogOp<'next>) ->
                    let state = effect.String :: acc
                    let next = effect.Next ()
                    state, next
            Final = List.rev
        }
*)
