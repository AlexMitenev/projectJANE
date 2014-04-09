module StaticAnalysis

open AST
open Errors

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ //////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Формирует по списку из имён узлов и их позиций ошибки дублирования узлов
let private DuplicateNode (listNodes : (string * Position) list) (kindNode : string) =
    
    // В отсортированном списке ищет повторы и формирует ошибки
    let rec searchDuplicate (listNodes : (string * Position) list) (prev : string) (acc : Error list) =
        match listNodes with
            | (name, p) :: lns when name = prev -> searchDuplicate lns name (Error.DuplicateNode kindNode name p :: acc)
            | (name, _) :: lns                  -> searchDuplicate lns name acc
            | []                                -> acc

    // Сортирует список по именам
    let sortwdListNodes = List.sortBy fst listNodes

    // Проверяет сотсортированный список на пустоту и начинает поиск повторов
    match sortwdListNodes with
    | (name, _) :: lns -> searchDuplicate lns name []
    | []               -> []

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Формирует ошибки корректности идентификатора
let private IncorrectNameErrors (name : string) (p : Position) =
    
    // Список запрещённых идентификаторов
    let listIncorrectNames = ["class"; "interface"; "extends"; "implements"; "static"; "final"; "if"; "else"; 
                              "while"; "for"; "break"; "continue"; "return"; "super"; "this"; "null"; "new";
                              "byte"; "short"; "int"; "long"; "float"; "double"; "char"; "string"; "boolean";
                              "false"; "true"; "instanceOf"; "void"]

    // Проверка на корректность
    let incorrectName = List.tryFind ((=) name) listIncorrectNames

    // Формирование ошибки
    match incorrectName with
    | Some str -> [Error.IncorrectName(str, p)]
    | None     -> []

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////// АНАЛИЗАТОРЫ //////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let SA_ProgramClass (p : Program) (c : Class) = [] // Заглушка

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let SA_ProgramInterface (p : Program) (i : Interface) = 

    // Ошибки о дублировании названий члена класса
    let DuplicateClassMemberErrors = 
        DuplicateNode (List.map (fun (im : InterfaceMember) -> im.Name.Value, im.Position) i.Members) "interface member"   
    


    DuplicateClassMemberErrors

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////


let SA_ProgramMember (p : Program) (pm : ProgramMember) =
    
    // Корректное ли название
    let incorrectNameError = IncorrectNameErrors pm.Name.Value pm.Position

    // Ошибки с нижних уровней
    let otherErrors = match pm with
                      | :? Class     as c -> SA_ProgramClass     p c
                      | :? Interface as i -> SA_ProgramInterface p i
                      | _                 -> []

    // Объединение ошибок в один список
    incorrectNameError @
    otherErrors

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////


let SA_Program(p : Program) = 
    
    // Ищет MainClass
    let mainClass = List.tryFind (fun (c : Class) -> c.Name.Value = p.NameMainClass) p.Classes
    
    // ЕслиMainClass найден, то ищет в нём main
    let mainMethod =
        match mainClass with
        | Some mc -> List.tryFind (fun (cm : ClassMethod) -> cm.Name.Value = "main") mc.Methods
        | None    -> None

    // Ошибки о дублировании названий классов
    let DuplicateClassErrors = 
        DuplicateNode (List.map (fun (pm : ProgramMember) -> pm.Name.Value, pm.Position) p.ProgramMembers) "class"   

    // Ошибки, связанные с main
    let mainErrors =
        match mainClass with
        | None    -> [Error.MainClassNotFound p] // Отсутствует main класс
        | Some mc -> match mainMethod with       
                     | None    -> [Error.MainNotFound mc] // В main классе отсутствует метод main
                     | Some mm -> 
                         // main не void метод
                         if not (mm :? ClassVoidMethod)             then [Error.MainIsNotVoid mm]           else []
                         // main не статический метод
                         |> fun errs -> if not mm.IsStatic          then Error.MainIsNotStatic mm :: errs   else errs
                         // main содержит параметры
                         |> fun errs -> if mm.Parameters.Length > 0 then Error.MainContaintsArgs mm :: errs else errs

    // Ошибки с нижних уровней
    let OtherErrors = List.collect (SA_ProgramMember p) p.ProgramMembers

    // Объединение ошибок в один список + метод main
    mainErrors           @ 
    DuplicateClassErrors @
    OtherErrors          , mainClass
    
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

