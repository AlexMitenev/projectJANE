namespace AST

[<AbstractClass>]
type Statement(pos : Position) =
    inherit Node(pos)
    abstract member Interpret: unit -> unit

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type Block(statements : Statement list, pos : Position) =
    inherit Statement(pos)
    member x.Statements = statements
    //Interpret all statement in block

    override x.ToString() = statements
                            |> List.map string
                            |> String.concat ""
                            |> sprintf "{\n%s}\n" 

    override x.Interpret() = List.iter (fun (s : Statement) -> s.Interpret()) statements

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type DeclarationStatement(declarationType : Type, name : ID, body : Initializer, pos : Position) =
    inherit Statement(pos)
    member x.Type = declarationType
    member x.Name = name
    member x.Body = body
    //add variable to the context

    override x.ToString() = sprintf "%A %A = %A;\n" declarationType name body

    override x.Interpret() = () // do this

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type AssignmentStatement(path : string list, body : Initializer, pos : Position) =
    inherit Statement(pos)
    member x.Path = path
    member x.Body = body
    //find variable in context and change value

    override x.ToString() = sprintf "%s = %A;\n" (String.concat "." path) body

    override x.Interpret() = () // do this

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type MemberCallStatement(body : Expression, pos : Position) =
    inherit Statement(pos)
    member x.Body = body
    
    override x.ToString() = sprintf "%A;\n" body

    override x.Interpret() = () // later

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type IfStatement(condition : Expression, trueStatement : Statement, falseStatement : Statement option, pos : Position) =
    inherit Statement(pos)
    member x.Condition      = condition
    member x.TrueStatement  = trueStatement
    member x.FalseStatement = falseStatement
    
    override x.ToString() = let ifPart = sprintf "if (%A) %A" condition trueStatement
                            match falseStatement with
                            | None           -> ifPart
                            | Some statement -> sprintf "%s\nelse %A" ifPart statement

    //if condition is true - interpret trueStatement, else - falseStatement if it present
    override x.Interpret() =
        if condition.Interpret().Bool.Value 
            then trueStatement.Interpret()
            else 
                 if falseStatement.IsSome 
                 then falseStatement.Value.Interpret()

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type WhileStatement(condition : Expression, body : Statement, pos : Position) =
    inherit Statement(pos)
    member x.Condition = condition
    member x.Body      = body

    //while condition is true - interpret body

    override x.ToString() = sprintf "while (%A) %A" condition body

    override x.Interpret() =
        while condition.Interpret().Bool.Value do
            body.Interpret()

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type ForStatement(init : DeclarationStatement, condition : Expression, 
                  update : AssignmentStatement, body : Statement, pos : Position) =
    inherit Statement(pos)
    member x.Init      = init
    member x.Condition = condition
    member x.Update    = update
    member x.Body      = body

    override x.ToString() = sprintf "for (%A %A = %A; %A; %s = %A) %A" init.Type init.Name init.Body 
                                    condition (String.concat "." update.Path) update.Body body

    //interpret init, while condition is true interpret body and interpret update
    override x.Interpret() =   
        init.Interpret()    
        while condition.Interpret().Bool.Value do
            body.Interpret()
            update.Interpret()
                            

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type BreakStatement(pos : Position) =
    inherit Statement(pos)

    override x.ToString() = "break;"

    override x.Interpret() = () // later

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type ContinueStatement(pos : Position) =
    inherit Statement(pos)

    override x.ToString() = "continue;"

    override x.Interpret() = () // later

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type ReturnStatement(expression : Expression option, pos : Position) =
    inherit Statement(pos)
    member x.Expression = expression

    override x.ToString() = sprintf "return %A;" expression

    override x.Interpret() = () //later

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type SuperStatement(arguments : Arguments, pos : Position) =
    inherit Statement(pos)
    member x.Arguments = arguments

    override x.ToString() = sprintf "super%A;" arguments

    override x.Interpret() = () // later
