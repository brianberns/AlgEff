namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type PureStateEnv<'state, 'ret>(initial : 'state) as this =
    inherit Environment<'ret>()

    let handler = PureStateHandler(initial, this)
    
    interface StateContext<'state>

    member _.Handler = handler

[<TestClass>]
type StateTest() =

    [<TestMethod>]
    member _.State() =

        let program =
            effect {
                let! x = State.get
                do! State.put (x + 1)
                let! y = State.get
                do! State.put (y + y)
                let! z = State.get
                return z.ToString()
            }

        let result, state =
            PureStateEnv(1).Handler.Run(program)
        Assert.AreEqual("4", result)
        Assert.AreEqual(4, state)
