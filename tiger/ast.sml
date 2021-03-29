structure AST = struct
  
type variableID = string
type typeID = string

datatype ast = expressions of exp
             | declarations of dec list

and ty = typeAlias of typeID 
        | recordType of tyfield list 
        | arrayType of typeID 

and tyfield = fields of {id: variableID, type_id: typeID}

and exp = NULL
        | integerExp of int
        | stringExp of string
        | arrayCreation of {type_id: typeID, size: exp, value: exp}
        | recordCreation of {type_id: typeID, assignments: {varname:variableID, value: exp} list}
        | variableExp of lvalue
        | funCall of {varID: variableID, args: exp list}
        | negExp of exp
        | Oper of {leftexp: exp, oper: operation, rightexp: exp}
        | exps of exp list
        | assignment of {lhs: lvalue, rhs: exp}
        | ifCond of {check:exp, thenCond: exp, elseCond: exp option}
        | whileCond of {check:exp, doCond: exp}
        | forCond of {check: variableID, equalto: exp, limit: exp, doCond: exp}
        | letCond of {declist: dec list, explist: exp list}

and lvalue = variable of variableID
            | objectReference of {varname:lvalue, instance: variableID}
            | arrayAccess of {arrname:lvalue, arrelement:exp} 

and dec = typeDec of {id: variableID, type_id: ty}
        | varDec of {varID: variableID, type_id: typeID option, rhs: exp} 
        | funDec of {varID: variableID, fields: tyfield list, type_id: typeID option, value: exp}
        | primitiveDec of {varID: variableID, fields: tyfield list, type_id: typeID option}

and operation = Plus
        | Minus
        | Mul
        | Div
        | Equal
        | LeT
        | GrT
        | Leq
        | Geq
        | Neq 
        | And
        | Or

end