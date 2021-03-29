structure PP : sig
   val compile : AST.ast -> string
   end
   = struct

    val indent : int ref = ref 0;


fun increase_level indent_ref = indent_ref := !indent_ref + 1
fun decrease_level indent_ref = indent_ref := !indent_ref - 1

fun form_indent n = if n = 0 then ""
                    else " " ^ form_indent (n-1);

fun get_indent () = let 
                val y = !indent
                in 
                form_indent y
                end

fun colorCode code str = if code = "typeid" then String.str(chr 27) ^ "[1;33m" ^ str ^ String.str(chr 27) ^ "[0;37m"
                        else if code = "conditional" then String.str(chr 27) ^ "[1;34m" ^ str ^ String.str(chr 27) ^ "[0;37m"
                        else if code = "literal" then String.str(chr 27) ^ "[1;32m" ^ str ^ String.str(chr 27) ^ "[0;37m"
                        else if code = "keyword" then String.str(chr 27) ^ "[1;35m" ^ str ^ String.str(chr 27) ^ "[0;37m"
                        else ""

fun compile (AST.expressions exp)        = (pretty_exp exp) ^ "\n" 
    | compile (AST.declarations declist) = (pretty_decs declist) 




   and pretty_exp (AST.NULL) = "nil"
   | pretty_exp (AST.integerExp iexp) = colorCode "literal" (Int.toString(iexp) ^ "")
   | pretty_exp (AST.stringExp sexp) = colorCode "literal" sexp
   | pretty_exp (AST.funCall fcall)  = let

                                        val {varID, args} = fcall
                                        val funName = varID
                                        val arguments = pretty_arglist args

                                        in

                                            funName ^ "(" ^ arguments ^ ")" 

                                        end

   | pretty_exp (AST.Oper oper) = let

                                    val {leftexp, oper, rightexp} = oper
                                    val operation = pretty_oper oper

                                in

                                        (pretty_exp leftexp) ^ operation ^ (pretty_exp rightexp)
                                end

   | pretty_exp (AST.variableExp lval) = pretty_lvalue lval
   | pretty_exp (AST.assignment asgn)  = let 
                                        val {lhs, rhs} = asgn
                                        in
                                        pretty_lvalue lhs ^ " := " ^ pretty_exp rhs 
                                        end

   | pretty_exp (AST.negExp nexp)  = " - " ^ pretty_exp nexp

   | pretty_exp (AST.exps explist) = pretty_exps explist

   | pretty_exp (AST.ifCond condition) = let

                                         val {check, thenCond, elseCond} = condition

                                         val y = !indent
                                         val else_ = form_indent y ^ "else\n"
                                         val else_keyword = colorCode "conditional" else_
                                         val if_keyword = form_indent y ^ colorCode "conditional" "if "
                                         val then_keyword = colorCode "conditional" " then\n"


                                         val _ = increase_level indent
                                         val y = !indent
                                         val thenCond_ = (form_indent y) ^ pretty_exp thenCond
                                         val elseCond_ = case elseCond of
                                             SOME (exp) => (form_indent y) ^ pretty_exp exp
                                             | NONE => ""

                                         val _ = decrease_level indent                                       
                                         in


                                            case elseCond of

                                            SOME (exp) => "\n" ^ if_keyword ^ (pretty_exp check) ^ then_keyword ^ thenCond_ ^ "\n" ^ else_keyword ^ (elseCond_)
                                            | NONE => "\n" ^ if_keyword ^ (pretty_exp check) ^ then_keyword ^ thenCond_
                                            

                                         end

                                        

   | pretty_exp (AST.whileCond condition) = let
                                            val {check, doCond} = condition
                                            
                                            val while_keyword = colorCode "conditional" "while "
                                            val do_keyword = colorCode "conditional" " do\n"

                                            val _ = increase_level indent
                                            val y = !indent
                                            val doCond_ = (form_indent y) ^ pretty_exp doCond


                                            val _ = decrease_level indent
                                            in
                                            
                                            while_keyword ^ (pretty_exp check) ^ do_keyword ^ doCond_

                                            end
   | pretty_exp (AST.forCond condition) = let
                                         val {check, equalto, limit, doCond} = condition

                                        val for_keyword = colorCode "conditional" "for "
                                        val do_keyword = colorCode "conditional" " do\n"
                                        val to_keyword = colorCode "conditional" " to "
                                            

                                         val _ = increase_level indent
                                         val y = !indent
                                         val doCond_ = (form_indent y) ^ pretty_exp doCond

                                         val _ = decrease_level indent

                                         in

                                         for_keyword ^ check ^ " := " ^ (pretty_exp equalto) ^ to_keyword ^ (pretty_exp limit) ^ do_keyword ^ doCond_   
                                        
                                        end

  
   | pretty_exp (AST.letCond condition) = let

                                        val {declist, explist} = condition
                                        


                                        val y = !indent
                                        val indent_in = (form_indent y) ^ "in \n" 
                                        val indent_end = (form_indent y) ^ "end"

                                        val let_keyword = colorCode "conditional" "let\n"
                                        val in_keyword = colorCode "conditional" indent_in
                                        val end_keyword = colorCode "conditional" indent_end


                                        val _ = increase_level indent
                                        
                                        val declist_ = pretty_decs declist
                                        val explist_ = pretty_exps explist
                                        
                                        val _ = decrease_level indent  
                                        in

                                        let_keyword ^ declist_ ^ in_keyword ^ explist_ ^ "\n" ^ end_keyword 
                                        
                                        end

   | pretty_exp (AST.arrayCreation arrCreate) = let
                                                val {type_id, size, value} = arrCreate
                                                in
                                                    type_id ^ "[" ^ (pretty_exp size) ^ "] " ^ "of " ^ (pretty_exp value)
                                                end

  | pretty_exp (AST.recordCreation recCreate) = let
                                                val {type_id, assignments} = recCreate
                                                val record_list = record_assignment_list assignments
                                                in
                                                    type_id ^ " { " ^ record_list ^ " } "
                                                end  


    and record_assignment record = let
                                    val {varname, value} = record
                                    in
                                    varname ^ " = " ^ pretty_exp value
                                    end

    and record_assignment_list [] = ""
    |   record_assignment_list [x] = record_assignment x
    |   record_assignment_list (x::xs) = record_assignment x ^ ", " ^ record_assignment_list xs
                                                

   and pretty_arglist [] = ""
   | pretty_arglist [x]  = pretty_exp x
   | pretty_arglist (x::xs) = (pretty_exp x) ^ ", " ^ (pretty_arglist xs)

   and pretty_oper AST.Plus = " + "
	| pretty_oper AST.Minus = " - "
	| pretty_oper AST.Mul =   " * "
	| pretty_oper AST.Div =   " / "
	| pretty_oper AST.Equal = " = "
	| pretty_oper AST.Neq =   " != "
	| pretty_oper AST.GrT =   " > "
	| pretty_oper AST.LeT =   " < "
	| pretty_oper AST.Geq =   " >= "
	| pretty_oper AST.Leq =   " <= "
	| pretty_oper AST.And =   " & "
	| pretty_oper AST.Or =    " | "


    and pretty_lvalue (AST.variable v) = v
    | pretty_lvalue (AST.objectReference obj) = let
                                                val {varname, instance} = obj
                                                in
                                                (pretty_lvalue varname) ^ "." ^ instance
                                                end
    | pretty_lvalue (AST.arrayAccess arr) = let
                                            val {arrname, arrelement} = arr
                                            in
                                            (pretty_lvalue arrname) ^ "[" ^ (pretty_exp arrelement) ^ "]"
                                            end

    and pretty_dec (AST.typeDec tyDecl) = let
                                        val {id, type_id} = tyDecl
                                        val type_keyword = colorCode "keyword" "type "
                                        in
                                       type_keyword ^ id ^ " = " ^ (pretty_ty type_id)
                                        end  
    | pretty_dec (AST.varDec varDecl) = let
                                        val {varID, type_id, rhs} = varDecl
                                        val var_keyword = colorCode "keyword" "var "
                                        in
                                        case type_id of
                                        SOME (ty) => var_keyword ^ varID ^ " : " ^ colorCode "typeid" ty ^ " := " ^ (pretty_exp rhs)
                                        | NONE => var_keyword ^ varID ^ " := " ^ (pretty_exp rhs)

                                        end
    | pretty_dec (AST.funDec funDecl) = let

                                        val {varID, fields, type_id, value} = funDecl
                                        val _ = increase_level indent
                                        val y = !indent
                                        val value_ = (form_indent y) ^ pretty_exp value
                                        val keyword = colorCode "keyword" "function "

                                        val _ = decrease_level indent  
                                        in

                                        case type_id of
                                        SOME (ty) => keyword ^ varID ^ "(" ^ (pretty_tyfieldlist fields) ^ ")" ^ " : " ^ colorCode "typeid" ty ^ " = \n" ^  value_
                                        | NONE => keyword ^ varID ^ "(" ^ (pretty_tyfieldlist fields) ^ ")" ^ " = \n" ^ value_

                                        end

    | pretty_dec (AST.primitiveDec primDecl) = let

                                        val {varID, fields, type_id} = primDecl
                                        val primitive_ = colorCode "keyword" "primitive "
                                        in

                                        case type_id of
                                        SOME (ty) => primitive_ ^ varID ^ "(" ^ (pretty_tyfieldlist fields) ^ ")" ^ " : " ^ colorCode "typeid" ty 
                                        | NONE => primitive_ ^ varID ^ "(" ^ (pretty_tyfieldlist fields) ^ ")"

                                        end


    and pretty_ty (AST.typeAlias typ) = typ
    |   pretty_ty (AST.recordType record) = " { " ^ pretty_tyfieldlist record ^ " } "
    |   pretty_ty (AST.arrayType arrtype) = let

                                            val arrayof_keyword = colorCode "keyword" "array of "
                                            val array_type = colorCode "typeid" arrtype
                                            in
                                                arrayof_keyword ^ array_type
                                            end

    and pretty_tyfield (AST.fields typfld) = let

                                val {id, type_id} = typfld
                                in

                                id ^ " : " ^ colorCode "typeid" type_id
                                end

    and  pretty_tyfieldlist [] = ""
    |   pretty_tyfieldlist [x] = pretty_tyfield x
    | pretty_tyfieldlist (x::xs) = pretty_tyfield x ^ ", " ^ pretty_tyfieldlist xs

    and pretty_exps [] = ""
    | pretty_exps [x] = get_indent() ^ pretty_exp x 
    | pretty_exps (exlist) = let

        val start =  "(\n";
        val end_string   = get_indent() ^ ")";

        val _ = increase_level indent
        fun explist [] = ""
            | explist (x::xs) = get_indent() ^ pretty_exp x ^ ";\n" ^ explist xs

        val final_str = explist exlist 

        val _ = decrease_level indent

        in
            start ^ final_str ^ end_string
        end

   and pretty_decs [] = ""
        | pretty_decs (x :: xs) = get_indent() ^ (pretty_dec x) ^ "\n" ^ (pretty_decs xs)

end