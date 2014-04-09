module Test

open AST
open StaticAnalysis
open Errors

//class myClass {
//
//    myClass() {
//    }
//
//    static int main() {
//        string helloWorld = "Hello";
//        helloWorld = helloWorld + " World!"
//        stdout.print(helloWorld);
//    }
//
//}

/// p              - фиктивная нулевая позиция
let p              = new Position(0, 0, 0, 0)

let myTypeString   = new StringType(0, p)

let myHello        = new StringLiteral("Hello", p)
let myDecl         = new DeclarationStatement(myTypeString, new ID("helloWorld", p), myHello, p)

let myWorld        = new StringLiteral(" World!", p)
let myMemberCall1  = new BinaryOperation(new Identifier(new ID("helloWorld", p)), ADDITION, myWorld, p)
let myAssign       = new AssignmentStatement(["helloWorld"], myMemberCall1, p)
                           
let myMember       = new Member(new ID("print", p), new Arguments([new Identifier(new ID("helloWorld", p))], p), p)
let myMemberCall2  = new BinaryOperation(new Identifier(new ID("stdout", p)), MEMBER_CALL, myMember, p)
let myPrint        = new MemberCallStatement(myMemberCall2, p)

let myBlock        = new Block([myDecl; myAssign; myPrint], p)
let myMethod       = new ClassVoidMethod(true, new ID("main", p),[], myBlock, p)

let myClassMembers = List.map (fun a -> a :> ClassMember) [myMethod]
let myConstructor  = new ClassConstructor(new ID("myClass", p), [], new Block([], p), p)
let myClass        = new Class(new ID("myClass", p), None, [], Some myConstructor, myClassMembers, p)
let myClasses      = List.map (fun a -> a :> ProgramMember) [myClass]
let myProg         = new Program(myClasses, "myClass", p)

let err, main = SA_Program myProg

printfn "%A" myProg