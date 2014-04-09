namespace AST

[<AbstractClass>]
type ClassMember(name : ID, pos : Position) =
    inherit Node(pos)
    member x.Name = name   

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type ClassMethodOrField(isStatic : bool, name : ID, pos : Position) =
    inherit ClassMember(name, pos)
    member x.IsStatic = isStatic

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

[<AbstractClass>]
type ClassMethod(isStatic : bool, name : ID, parameters : FormalParameter list, body : Block, pos : Position) =
    inherit ClassMethodOrField(isStatic, name, pos)
    member x.Parameters = parameters
    member x.Body       = body

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type ClassReturnMethod(isStatic : bool, returnType : Type, name : ID, 
                       parameters : FormalParameter list, body : Block, pos : Position) =
    inherit ClassMethod(isStatic, name, parameters, body, pos)
    member x.ReturnType = returnType

    override x.ToString() = 
        let staticStr = if isStatic then "static " else ""
        let parametersStr = parameters |> List.map string |> String.concat ", " |> sprintf "(%s)"
        sprintf "%s%A %s%s %A" staticStr returnType name.Value parametersStr body

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type ClassVoidMethod(isStatic : bool, name : ID, parameters : FormalParameter list, body : Block, pos : Position) =
    inherit ClassMethod(isStatic, name, parameters, body, pos)

    override x.ToString() = 
        let staticStr     = if isStatic then "static " else ""
        let parametersStr = parameters |> List.map string |> String.concat ", " |> sprintf "(%s)"
        sprintf "%svoid %s%s %A" staticStr name.Value parametersStr body

    //Interpret body of method
    member x.Interpret() = 
        (body :> Statement).Interpret()

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type ClassField(isStatic : bool, isFinal : bool, fieldType : Type, name : ID, body : Expression, pos : Position) =
    inherit ClassMethodOrField(isStatic, name, pos)
    member x.Type    = fieldType
    member x.Body    = body
    member x.IsFinal = isFinal 
   
    override x.ToString() = 
        let staticStr = if isStatic then "static " else ""
        let finalStr  = if isStatic then "final "  else ""
        sprintf "%s%s%A %s = %A;" staticStr finalStr fieldType name.Value body

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type ClassConstructor(name : ID, parameters : FormalParameter list, body : Block, pos : Position) =
    inherit ClassMember(name, pos)
    member x.Parameters = parameters
    member x.Body       = body

    override x.ToString() = 
        let parametersStr = parameters |> List.map string |> String.concat ", " |> sprintf "(%s)"

        sprintf "%s %s %A" name.Value parametersStr body