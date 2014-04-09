namespace AST

[<AbstractClass>]
type Primary(pos) =
    inherit Expression(pos)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type Constructor(typeName : ID, arguments : Arguments, pos : Position) =
    inherit Primary(pos)
    member x.Name      = typeName
    member x.Arguments = arguments

    override x.ToString() = sprintf "new %A%A" typeName arguments

    override x.Interpret() = new Val() // later

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type Identifier(name : ID) =
    inherit Primary(name.Position)
    member x.Name = name

    override x.ToString() = name.ToString()
 
    override x.Interpret() = new Val() // do this

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type Member(name : ID, suffix : Suffix, pos : Position) =
    inherit Primary(pos)
    member x.Name   = name
    member x.Suffix = suffix

    override x.ToString() = sprintf "%s%A" name.Value suffix

    override x.Interpret() = new Val() // do this