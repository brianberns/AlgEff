namespace AlgEff

(*
type Effect<'a> =
    | Log of (string * (unit -> 'a))
*)

type Effect<'a> =
    abstract member Map : ('a -> 'b) -> Effect<'b>

type AlgEffProgram<'a> =
    | Free of Effect<AlgEffProgram<'a>>
    | Pure of 'a

module AlgEffProgram =

    let rec bind f = function
        | Free effect ->
            effect.Map(bind f) |> Free
        | Pure x -> f x

type AlgEffBuilder() =
    member this.Bind(x, f) = AlgEffProgram.bind f x
    member this.Return(x) = Pure x
    member this.ReturnFrom(x) = x
    member this.Zero() = Pure ()

[<AutoOpen>]
module AutoOpen =

    let effect = AlgEffBuilder()

type Log<'a>(str: string, next : unit -> 'a) =
    interface Effect<'a> with
        member __.Map(f) =
            Log(str, next >> f) :> _
    member __.String = str
    member __.Next = next

module Log =

    let write str = Free (Log(str, Pure))

    let writef fmt = Printf.ksprintf write fmt

    let handle<'a> =
        {|
            Init = List.empty<string>
            Step =
                fun acc (effect : Log<'a>) ->
                    let state = effect.String :: acc
                    let next = effect.Next ()
                    state, next
            Final = List.rev
        |}

type WriteLine<'a>(str: string, next : unit -> 'a) =
    interface Effect<'a> with
        member __.Map(f) =
            WriteLine(str, next >> f) :> _
    member __.String = str
    member __.Next = next

module Console =

    let writeln str = Free (WriteLine(str, Pure))

    let writelnf fmt = Printf.ksprintf writeln fmt

    let handle<'a> =
        {|
            Init = List.empty<string>
            Step =
                fun acc (effect : WriteLine<'a>) ->
                    let state = effect.String :: acc
                    let next = effect.Next ()
                    state, next
            Final = List.rev
        |}
