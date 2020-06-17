namespace AlgEff

type Effect<'a> =
    abstract member Map : ('a -> 'b) -> Effect<'b>

type AlgEffProgram<'ctx, 'a> =
    | Free of Effect<AlgEffProgram<'ctx, 'a>>
    | Pure of 'a

module AlgEffProgram =

    let rec bind (f : 'a -> AlgEffProgram<'ctx, 'b>) (program : AlgEffProgram<'ctx, 'a>) =
        match program with
        | Free effect ->
            effect.Map(bind f) |> Free
        | Pure x -> f x

type AlgEffBuilder() =
    member __.Bind(x, f) = AlgEffProgram.bind f x
    member __.Return(x) = Pure x
    member __.ReturnFrom(x) = x
    member __.Zero() = Pure ()

[<AutoOpen>]
module AutoOpen =

    let effect = AlgEffBuilder()

type Log<'a>(str: string, next : unit -> 'a) =
    interface Effect<'a> with
        member __.Map(f) =
            Log(str, next >> f) :> _
    member __.String = str
    member __.Next = next

type Log = interface end

module Log =

    let write<'ctx when 'ctx :> Log> str : AlgEffProgram<'ctx, _> =
        Free (Log(str, Pure))

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

type ReadLine<'a>(next : string -> 'a) =
    interface Effect<'a> with
        member __.Map(f) =
            ReadLine(next >> f) :> _
    member __.Next = next

type Console = interface end

module Console =

    let writeln<'ctx when 'ctx :> Console> str : AlgEffProgram<'ctx, _> =
        Free (WriteLine(str, Pure))

    let writelnf fmt = Printf.ksprintf writeln fmt

    let readln<'ctx when 'ctx :> Console> : AlgEffProgram<'ctx, _> =
        Free (ReadLine(Pure))

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
