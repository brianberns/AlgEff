namespace AlgEff.Effect

/// Base console effect type.
[<AbstractClass>]
type ConsoleEffect<'next>() =
    inherit Effect<'next>()

    /// Type-safe subtype enumeration.
    abstract member Case : ConsoleEffectSum<'next>

/// Writes the given string to the console.
and WriteLineEffect<'next>(str : string, cont : unit -> 'next) =
    inherit ConsoleEffect<'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        WriteLineEffect(str, cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = WriteLine this

    /// String to write.
    member __.String = str

    /// Continuation to next effect.
    member __.Cont = cont

/// Reads a line from the console.
and ReadLineEffect<'next>(cont : string -> 'next) =
    inherit ConsoleEffect<'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        ReadLineEffect(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = ReadLine this

    /// Continuation to next effect.
    member __.Cont = cont

/// Sum type for console effects.
and ConsoleEffectSum<'next> =
    | WriteLine of WriteLineEffect<'next>
    | ReadLine of ReadLineEffect<'next>

/// Console context requirement.
type ConsoleContext = interface end

module Console =

    /// Writes the given line to the console.
    let writeln<'ctx when 'ctx :> ConsoleContext> str : EffectChain<'ctx, _> =
        Free (WriteLineEffect(str, Pure))

    /// Formats and writes a line to the console.
    let writelnf fmt = Printf.ksprintf writeln fmt

    /// Reads a line from the console.
    let readln<'ctx when 'ctx :> ConsoleContext> : EffectChain<'ctx, _> =
        Free (ReadLineEffect(Pure))
