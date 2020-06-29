namespace AlgEff.Handler

open AlgEff.Effect

type PickTrue<'ctx, 'res when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'res>>(context : 'ctx) =
    inherit Handler<'ctx, 'res, Unit, Unit>()

    override __.Start = Unit

    override __.TryStep(_, effect, cont) =
        match effect with
            | :? NonDetEffect<Program<'ctx, 'res>> as nonDetEff ->
                match nonDetEff.Case with
                    | Decide eff ->
                        let next = eff.Cont(true)
                        cont Unit next
                    |> Some
            | _ -> None

    override __.Finish(Unit) = Unit

type PickMax<'ctx, 'res when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'res> and 'res : comparison>(context : 'ctx) =
    inherit Handler<'ctx, 'res, Unit, Unit>()

    override __.Start = Unit

    override __.TryStep(_, effect, cont) =
        match effect with
            | :? NonDetEffect<Program<'ctx, 'res>> as nonDetEff ->
                match nonDetEff.Case with
                    | Decide eff ->
                        let outStateT, resT = eff.Cont(true) |> cont Unit
                        let outStateF, resF = eff.Cont(false) |> cont Unit
                        if resT > resF then
                            outStateT, resT
                        else
                            outStateF, resF
                    |> Some
            | _ -> None

    override __.Finish(Unit) = Unit
