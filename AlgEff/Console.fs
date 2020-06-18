namespace AlgEff

type WriteLineOp<'next>(str: string, next : unit -> 'next) =
    interface Op<'next> with
        member __.Map(f) =
            WriteLineOp(str, next >> f) :> _
    member __.String = str
    member __.Next = next

type ReadLineOp<'next>(next : string -> 'next) =
    interface Op<'next> with
        member __.Map(f) =
            ReadLineOp(next >> f) :> _
    member __.Next = next

type ConsoleEffect<'a> =
    | WriteLine of WriteLineOp<'a>
    | ReadLine of ReadLineOp<'a>

type ConsoleHandler =
    abstract member GetHandler<'a> : unit -> EffectHandler<ConsoleEffect<'a>>

module Console =

    let writeln<'ctx when 'ctx :> ConsoleHandler> str : OpChain<'ctx, _> =
        Free (WriteLineOp(str, Pure))

    let writelnf fmt = Printf.ksprintf writeln fmt

    let readln<'ctx when 'ctx :> ConsoleHandler> : OpChain<'ctx, _> =
        Free (ReadLineOp(Pure))

module ConsoleHandler =

    type State =
        {
            Input : List<string>
            Output : List<string>
        }

    module State =

        let create input =
            {
                Input = input
                Output = []
            }

    let handle<'a> input : EffectHandlerImpl<State, ConsoleEffect<'a>, 'a> =

        let handleEffect state = function
            | WriteLine op ->
                let state' =
                    { state with Output = op.String :: state.Output }
                let next = op.Next ()
                state', next
            | ReadLine op ->
                match state.Input with
                    | head :: tail ->
                        let state' =
                            { state with Input = tail }
                        let next = op.Next head
                        state', next
                    | _ -> failwith "No more nput"

        let final state =
            { state with Output = state.Output |> List.rev }

        {
            Init = State.create input
            Step = handleEffect          
            Final = final
        }
