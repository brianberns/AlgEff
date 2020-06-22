namespace AlgEff.Effect

/// Base console effect type.
[<AbstractClass>]
type ConsoleEff<'next>() =
    inherit Effect<'next>()

    /// Type-safe subtype enumeration.
    abstract member Case : ConsoleEffSum<'next>

/// Writes the given string to the console.
and WriteLineEff<'next>(str : string, cont : unit -> 'next) =
    inherit ConsoleEff<'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        WriteLineEff(str, cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = WriteLine this

    /// String to write.
    member __.String = str

    /// Continuation to next effect.
    member __.Cont = cont

/// Reads a line from the console.
and ReadLineEff<'next>(cont : string -> 'next) =
    inherit ConsoleEff<'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        ReadLineEff(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = ReadLine this

    /// Continuation to next effect.
    member __.Cont = cont

/// Sum type for console effects.
and ConsoleEffSum<'next> =
    | WriteLine of WriteLineEff<'next>
    | ReadLine of ReadLineEff<'next>

/// Console context requirement.
type ConsoleContext = interface end

module Console =

    /// Writes the given line to the console.
    let writeln<'ctx when 'ctx :> ConsoleContext> str : EffectChain<'ctx, _> =
        Free (WriteLineEff(str, Pure))

    /// Formats and writes a line to the console.
    let writelnf fmt = Printf.ksprintf writeln fmt

    /// Reads a line from the console.
    let readln<'ctx when 'ctx :> ConsoleContext> : EffectChain<'ctx, _> =
        Free (ReadLineEff(Pure))
