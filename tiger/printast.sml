structure PrintAbsyn  =
struct

  structure A = AST
  
  fun printSingle str = TextIO.output(TextIO.stdOut, str)
  fun printAll [] = []
  |	printAll (x :: xs) = (printSingle x) :: (printAll xs)

    and unroll_exp (A.integerExp literal) = ["integerExp(", Int.toString literal ,")"]
	| unroll_exp (A.stringExp literal) = ["stringExp(\"", literal ,"\")"]
    | unroll_exp A.NULL                = ["NIL"]

    | unroll_exp (A.arrayCreation arr) =    let
                                    val {type_id, size, value} = arr
									val typid = (type_id)
                                in
                                    ["arrayCreation({type_ = ", typid , ", length = "] @ (unroll_exp size) @ [", init = "] @ (unroll_exp value) @ ["})"]
                                end

	| unroll_exp (A.recordCreation record) =  let
                                    val {type_id, assignments} = record
                                    val record_list = record_assignment_list assignments
                                in
                                    ["Record({type_id = ", type_id , ", assignments = "] @ record_list @ ["})"]
                                end

    | unroll_exp (A.Oper operation) = let
                val {leftexp, oper, rightexp} = operation
                val opfield = oplist oper
            in
                ["Oper({leftexp = "] @ unroll_exp leftexp @ [", oper = "] @ opfield @ [", rightexp = "] @ unroll_exp rightexp @ ["})"]
            end
    

	| unroll_exp (A.variableExp lval) = ["variableExp("] @ unroll_lvalue lval @ [")"]
	| unroll_exp (A.funCall f)			 = let
                                            val {varID, args} = f
                                            val argslist = ["["] @  unroll_exps args @ ["]"]
                                        in
                                            ["funCall({varID = ", varID, ", args = "] @ argslist @ ["})"]
                                        end
	| unroll_exp (A.negExp nexp) = (["negExp("] @ unroll_exp nexp @ [")"])
 
	| unroll_exp (A.exps exs) = ["["] @  unroll_exps exs @ ["]"]
	| unroll_exp (A.assignment aeq)      =   let
                                            val {lhs, rhs} = aeq
                                            val lval = unroll_lvalue lhs
                                        in
                                            ["assignment({lhs = "] @ lval @ [", exp = "] @ unroll_exp rhs @ ["})"]
                                        end

	| unroll_exp (A.ifCond condition)    = let
                                        val {check, thenCond, elseCond} = condition
                                        val if_ = unroll_exp check
                                        val then_ = unroll_exp thenCond
                                    in
                                        case elseCond of
                                        SOME else_ => ["ifCond({check = "] @ if_ @ [", thenCond = "] @ then_ @ [", elseCond = "] @ (unroll_exp else_) @ ["})"]
                                        | NONE => ["ifCond({check = "] @ if_ @ [", thenCond = "] @ then_ @ [", elseCond = NONE"] @ ["})"]
                                    end

	| unroll_exp (A.whileCond condition) = let
                                    val {check, doCond} = condition
                                    val while_ = unroll_exp check
                                    val do_ = unroll_exp doCond
                                in
                                    ["whileCond({check = "] @ while_ @ [", doCond = "] @ do_ @ ["})"]
                                end
	| unroll_exp (A.forCond condition) = let
                                    val {check, equalto, limit, doCond} = condition
                                    val equalto_ = unroll_exp equalto
                                    val limit_ = unroll_exp limit
                                    val doCond_ = unroll_exp doCond
                                in
                                    ["forCond({check = ", check] @ [", equalto = "] @ equalto_ @ [", limit = "] @ limit_ @ [", doCond = "] @ doCond_ @ ["})"]
                                end


	| unroll_exp (A.letCond condition) = let
                                    val {declist, explist} = condition
                                    val first_ = ["["] @  unroll_decs declist @ ["]"]
                                    val second_ = ["["] @ unroll_exps explist @ ["]"]
                                in
                                    ["letCond({first = "] @ first_ @ [", second = "] @ second_ @ ["})"]
                                end
	
	

    and record_assignment record = let
                                val {varname, value} = record
                            in
                                ["{varname = ", varname, ", value = "] @ unroll_exp value @ ["}, "]
                            end

    and record_assignment_list [] = []
        | record_assignment_list (x::xs) = (record_assignment x) @ (record_assignment_list xs)


	and oplist A.Plus = ["Plus"]
	| oplist A.Minus = ["Minus"]
	| oplist A.Mul = ["Mul"]
	| oplist A.Div = ["Div"]
	| oplist A.Equal = ["Equal"]
	| oplist A.Neq = ["Neq"]
	| oplist A.GrT = ["GrT"]
	| oplist A.LeT = ["LeT"]
	| oplist A.Geq = ["Geq"]
	| oplist A.Leq = ["Leq"]
	| oplist A.And = ["And"]
	| oplist A.Or = ["Or"]


	and unroll_lvalue (A.variable varExp) = ["id(", varExp , ")"]
	| unroll_lvalue   (A.objectReference lval) = let
                                        val {varname, instance} = lval
                                        val varname_ = unroll_lvalue varname
                                    in
                                        ["objectReference({varname = "] @ varname_ @ [", instance = ", instance] @ ["})"]
                                    end

	| unroll_lvalue (A.arrayAccess arrEl) = let
                                            val {arrname, arrelement} = arrEl
                                            val arrname_ = unroll_lvalue arrname
                                            val arrelement_ = unroll_exp arrelement
                                        in
                                            ["arrayAccess({arrname = "] @ arrname_ @ [", arrelement = "] @ arrelement_ @ ["})"]
                        
						                end


    and unroll_tyfield (A.fields fld) =    let
                                val {id, type_id} = fld;
                            in
                                ["{", "id = ", id , ", type_id = ", (type_id), "}"]
                            end



	

    and unroll_dec (A.varDec variableDecl) =   let
                                    val {varID, type_id, rhs} = variableDecl;
                                    val rhs_ = unroll_exp rhs
                                in
                                    case type_id of
                                    SOME (typ) => (["varDec({varID = ", varID, ", type_id = ", (typ), ", rhs = "] @ rhs_ @ ["})"])
                                    | NONE => (["varDec({varID = ",  varID, ", type_id = NONE", ", rhs = "] @ rhs_ @ ["})"])
                                end
        | unroll_dec (A.typeDec typeDecl) =    let
                                        val {id, type_id} = typeDecl
                                    in
                                        ["typeDec({id = ", id, ", type_id = "] @ unroll_ty type_id @ ["})"]
                                    end

        | unroll_dec (A.funDec f) = let
                                        val {varID, fields, type_id, value} = f
                                        val tyfields_ = ["["] @ unroll_tyfields fields @ ["]"]
                                        val value_ = unroll_exp value
                                    in
                                        case type_id of
                                        SOME (typ) => (["funDec({varID = ", varID, ", fields = "] @ tyfields_ @ [", type_id = ", (typ)] @ [", value = "] @ value_ @ ["})"])
                                        | NONE => (["funDec({varID = ", varID, ", fields = "] @ tyfields_ @ [", type_id = NONE"] @ [", value = "] @ value_ @ ["})"])
                                    end

        | unroll_dec (A.primitiveDec primDecl) = let
                                            val {varID, fields, type_id} = primDecl
                                            val tyfields_ = ["["] @ unroll_tyfields fields @ ["]"]
                                        in
                                            case type_id of
                                            SOME (typ) => (["primitiveDec({varID = ",  varID, ", fields = "] @ tyfields_ @ [", type_id = ",  (typ) ] @ ["})"])
                                            | NONE => (["primitiveDec({varID = ",  varID, ", fields = "] @ tyfields_ @ ["})"])
                                        end


    and unroll_ty (A.typeAlias typ) = ["typeAlias(", typ ,")"]
        | unroll_ty (A.recordType record) =    let
                                            val tyfields_ = ["["] @ unroll_tyfields record @ ["]"]
                                        in
                                            ["recordType("] @ tyfields_ @ [")"]
					                    end
        | unroll_ty (A.arrayType arr) = ["arrayType(", arr, ")"]
        

    and unroll_tyfields [] = []
        | unroll_tyfields (x::xs) =   (case xs of
                                        [] => (unroll_tyfield x) @ (unroll_tyfields xs)
                                        | _ => (unroll_tyfield x) @ [", "] @ (unroll_tyfields xs)
                                    )




    and unroll_exps [] = []
        | unroll_exps (y :: ys) = (case ys of 
                                        [] => (unroll_exp y) @ (unroll_exps ys)
                                        | _ => (unroll_exp y) @ [", "] @ (unroll_exps ys)
                                    )



    and unroll_decs [] = []
        | unroll_decs (x :: xs) = (case xs of 
                                        [] => (unroll_dec x) @ (unroll_decs xs)
                                        | _ => (unroll_dec x) @ [", "] @ (unroll_decs xs)
                                    )

    and printAST (A.expressions exp)			= (["expressions("] @ unroll_exp exp @ [")"] @ ["\n"])  
        | printAST (A.declarations declist) = (["declarations("] @ unroll_decs declist @ [")"] @ ["\n"])


    and printProgram prog = printAll (printAST prog) 

end