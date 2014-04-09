namespace AST

open System

[<AbstractClass>]
type ProgramMember(name : ID, pos : Position) =
    inherit Node(pos)
    member x.Name = name 

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type Interface(name : ID, ancestors : ID list, members : InterfaceMember list, pos : Position) =
    inherit ProgramMember(name, pos)
    
    let fields        = List.fold (fun acc (m : InterfaceMember) ->
                                       try m :?> InterfaceField :: acc 
                                       with :? InvalidCastException -> acc
                                  ) [] members

    let methods       = List.fold (fun acc (m : InterfaceMember) ->
                                       try m :?> InterfaceMethod :: acc 
                                       with :? InvalidCastException -> acc
                                  ) [] members 
    
    let returnMethods = List.fold (fun acc (m : InterfaceMethod) ->
                                       try m :?> InterfaceReturnMethod :: acc 
                                       with :? InvalidCastException -> acc
                                  ) [] methods 
 
    let voidMethods   = List.fold (fun acc (m : InterfaceMethod) ->
                                       try m :?> InterfaceVoidMethod :: acc 
                                       with :? InvalidCastException -> acc
                                  ) [] methods 

    member x.Ancestors     = ancestors
    member x.Members       = members  
    member x.Fields        = fields
    member x.Methods       = methods
    member x.ReturnMethods = returnMethods
    member x.VoidMethods   = voidMethods

    override x.ToString() =
        let extendsStr   = if not ancestors.IsEmpty then " extends " else ""
        let ancestorsStr = ancestors |> List.map string |> String.concat ", "
        let membersStr   = members |> List.map string |> String.concat "\n\n"
        sprintf "interface %A%s%s {\n\n%s\n\n}\n" name extendsStr ancestorsStr membersStr

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type Class(name : ID, ancestor : ID option, interfaces : ID list, 
           classConstructor : ClassConstructor option, members : ClassMember list, pos : Position) =
    inherit ProgramMember(name, pos)
    
    let fields        = List.fold (fun acc (m : ClassMember) ->
                                       try m :?> ClassField :: acc 
                                       with :? InvalidCastException -> acc
                                  ) [] members

    let methods       = List.fold (fun acc (m : ClassMember) ->
                                       try m :?> ClassMethod :: acc 
                                       with :? InvalidCastException -> acc
                                  ) [] members 
    
    let returnMethods = List.fold (fun acc (m : ClassMethod) ->
                                       try m :?> ClassReturnMethod :: acc 
                                       with :? InvalidCastException -> acc
                                  ) [] methods 
 
    let voidMethods   = List.fold (fun acc (m : ClassMethod) ->
                                       try m :?> ClassVoidMethod :: acc 
                                       with :? InvalidCastException -> acc
                                  ) [] methods
                                  
    let classConstructor = match classConstructor with
                           | Some cc -> cc
                           | None    -> let p = new Position(0,0,0,0)
                                        new ClassConstructor(name, [], new Block([], p), p)
    
    member x.Ancestor      = ancestor
    member x.Interfaces    = interfaces
    member x.Members       = members
    member x.Fields        = fields
    member x.Methods       = methods
    member x.ReturnMethods = returnMethods
    member x.VoidMethods   = voidMethods
    member x.Constructor   = classConstructor

    override x.ToString() =
        let extendsStr    = if ancestor.IsSome then sprintf " extends %A" ancestor.Value else ""
        let implementsStr = if not interfaces.IsEmpty then " implements " else ""
        let interfacesStr = interfaces |> List.map string |>  String.concat ", "
        let membersStr    = members |> List.map string |> String.concat "\n"
        sprintf "class %A%s%s%s {\n\n%A\n%s\n}\n" name extendsStr implementsStr interfacesStr classConstructor membersStr
