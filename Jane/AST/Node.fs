namespace AST

open System

type Position(startLine : int, startSymbol : int, endLine : int, endSymbol : int) =
    new((sLine, sSymbol), (eLine, eSymbol)) = 
        new Position(sLine, sSymbol, eLine, eSymbol)

    member x.StartLine   = startLine
    member x.StartSymbol = startSymbol
    member x.EndLine     = endLine
    member x.EndSymbol   = endSymbol
    member x.StartPos    = startLine, startSymbol
    member x.EndPos      = endLine, endSymbol
    
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type Node(pos : Position) = 
    let mutable pos = pos

    member x.Position with get() = pos
                      and set(p) = pos <- p

    member x.StartLine   = pos.StartLine
    member x.StartSymbol = pos.StartSymbol
    member x.EndLine     = pos.EndLine
    member x.EndSymbol   = pos.EndSymbol

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type ID(id : string, p : Position) =
    inherit Node(p)
    member x.Value = id

    override x.ToString() = id
