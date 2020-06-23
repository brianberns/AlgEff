namespace AlgEff

open AlgEff.Effect
open AlgEff.Handler

type NonDetConcreteContext<'res>(getHandler) as this =
    inherit ConcreteContext<'res>()
    
    let handler = this |> getHandler

    interface NonDetContext

    member __.Handler = handler

module Program =

    let program =
        (*
        NonDet.choose 15 30 >>= (fun x1 ->
            NonDet.choose 5 10 >>= (fun x2 ->
                Pure (x1 - x2)))
        *)
        effect {
            let! x1 = NonDet.choose 15 30
            let! x2 = NonDet.choose 5 10
            return x1 - x2
        }

    let result, () =
        NonDetConcreteContext(NonDetHandler.pickMax).Handler
            |> EffectHandler.run program
    printfn "%A" result
