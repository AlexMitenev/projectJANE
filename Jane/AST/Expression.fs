namespace AST

[<AbstractClass>]
type Expression(pos : Position) =
    inherit Initializer(pos)


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type InstanceOf(expression : Expression, controlType : Type, pos : Position) =
    inherit Expression(pos)
    member x.Expression  = expression
    member x.ControlType = controlType

    override x.ToString() = sprintf "(%A instanceOf %A)" expression controlType

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type BinaryOperation(firstOperand : Expression, operator : BinaryOperator, secondOperand : Expression, pos : Position) =
    inherit Expression(pos)
    member x.FirstOperand  = firstOperand
    member x.Operator      = operator
    member x.SecondOperand = secondOperand

    override x.ToString() = sprintf "(%A %s %A)" firstOperand (operator.ToString()) secondOperand

    override x.Interpret() =
        match operator with
        | OR               -> new Val(firstOperand.Interpret().Bool.Value || secondOperand.Interpret().Bool.Value)
        | AND              -> new Val(firstOperand.Interpret().Bool.Value && secondOperand.Interpret().Bool.Value)
        | EQUAL            -> new Val(firstOperand.Interpret().Bool.Value =  secondOperand.Interpret().Bool.Value)
        | NOT_EQUAL        -> new Val(firstOperand.Interpret().Bool.Value <> secondOperand.Interpret().Bool.Value)
        | GREATER          -> new Val(firstOperand.Interpret().Int.Value >  secondOperand.Interpret().Int.Value)
        | GREATER_OR_EQUAL -> new Val(firstOperand.Interpret().Int.Value >= secondOperand.Interpret().Int.Value)
        | LESS             -> new Val(firstOperand.Interpret().Int.Value <  secondOperand.Interpret().Int.Value)
        | LESS_OR_EQUAL    -> new Val(firstOperand.Interpret().Int.Value <= secondOperand.Interpret().Int.Value)
        | ADDITION         -> new Val(firstOperand.Interpret().Int.Value +  secondOperand.Interpret().Int.Value)
        | SUBSTRACTION     -> new Val(firstOperand.Interpret().Int.Value -  secondOperand.Interpret().Int.Value)
        | MULTIPLICATION   -> new Val(firstOperand.Interpret().Int.Value *  secondOperand.Interpret().Int.Value)
        | DIVISION         -> new Val(firstOperand.Interpret().Int.Value /  secondOperand.Interpret().Int.Value)
        | MODULUS          -> new Val(firstOperand.Interpret().Int.Value %  secondOperand.Interpret().Int.Value)
        | MEMBER_CELL      -> new Val() //later

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type UnaryOperation(operator : UnaryOperator, operand : Expression, pos : Position) =
    inherit Expression(pos)
    member x.Operator = operator
    member x.Operand  = operand

    override x.ToString() = sprintf "(%s %A)" (operator.ToString()) operand

    override x.Interpret() =
        match operator with
        | NOT              -> Val(not <| operand.Interpret().Bool.Value)
        | MINUS            -> Val(-      operand.Interpret().Int.Value)
