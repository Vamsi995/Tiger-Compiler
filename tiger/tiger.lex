type lineNo            = int
type pos               = lineNo  
val  lineRef : pos ref = ref 0   
				    

fun updateLine n      = lineRef := !(lineRef) + n



type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) Tokens.token

val nesting_comment = ref 0

fun incref ref_val = ref_val := !ref_val + 1
fun decref dec_val = dec_val := !dec_val - 1


fun lineRange l r = "line " ^ l
				 
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")

fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)


fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode

val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
%s COMMENT;
ws    = [\ \t];
digit = [0-9];
s_value = "\""([\\]. | [^"])*"\"";
legal_vars = [a-zA-Z0-9][a-zA-Z0-9_]*;

%%


<INITIAL> "/*" => (
    incref nesting_comment;
    YYBEGIN COMMENT;
    lex()
);

<COMMENT> "/*" => (
    incref nesting_comment;
    YYBEGIN COMMENT;
    lex()
);

<COMMENT> "*/" => (
    decref nesting_comment;
    if(!nesting_comment = 0) then
    (
        YYBEGIN INITIAL;
        lex()
    )
    else
        lex()
);

<COMMENT> . => ( lex() );

<COMMENT> \n => ( updateLine(newlineCount yytext); lex());

<INITIAL> \n => ( updateLine(newlineCount yytext); lex());



<INITIAL> {ws}+                           => ( lex() );
<INITIAL> \n({ws}*\n)*                    => ( updateLine (newlineCount yytext); lex());
<INITIAL> {s_value}                       => ( Tokens.STRING (yytext, !lineRef, !lineRef) );


<INITIAL> "array"                         => ( Tokens.ARRAY (!lineRef, !lineRef) );
<INITIAL> "of"                            => ( Tokens.OF (!lineRef, !lineRef) );
<INITIAL> "var"                           => ( Tokens.VAR (!lineRef, !lineRef) );
<INITIAL> "type"                          => ( Tokens.TYPE (!lineRef, !lineRef) );
<INITIAL> "if"                            => ( Tokens.IF (!lineRef, !lineRef) );
<INITIAL> "then"                          => ( Tokens.THEN (!lineRef, !lineRef) );
<INITIAL> "else"                          => ( Tokens.ELSE (!lineRef, !lineRef) );
<INITIAL> "while"                         => ( Tokens.WHILE (!lineRef, !lineRef) );
<INITIAL> "for"                           => ( Tokens.FOR (!lineRef, !lineRef) );
<INITIAL> "to"                            => ( Tokens.TO (!lineRef, !lineRef) );
<INITIAL> "do"                            => ( Tokens.DO (!lineRef, !lineRef) );
<INITIAL> "let"                           => ( Tokens.LET (!lineRef, !lineRef) );
<INITIAL> "in"                            => ( Tokens.IN (!lineRef, !lineRef) );
<INITIAL> "end"                           => ( Tokens.END (!lineRef, !lineRef) );
<INITIAL> "break"                         => ( Tokens.BREAK (!lineRef, !lineRef) );
<INITIAL> "function"                      => ( Tokens.FUNCTION (!lineRef, !lineRef) );
<INITIAL> "primitive"                     => ( Tokens.PRIMITIVE (!lineRef, !lineRef) );


<INITIAL> "+"                             => ( Tokens.PLUS  (!lineRef,!lineRef) );
<INITIAL> "-"                             => ( Tokens.MINUS  (!lineRef,!lineRef) );
<INITIAL> "*"                             => ( Tokens.MUL (!lineRef,!lineRef) );
<INITIAL> "/"                             => ( Tokens.DIV (!lineRef, !lineRef) );
<INITIAL> "="                             => ( Tokens.EQUAL (!lineRef, !lineRef) );
<INITIAL> "<>"                            => ( Tokens.NEQ (!lineRef, !lineRef) );
<INITIAL> ">"                             => ( Tokens.GT (!lineRef, !lineRef) );
<INITIAL> "<"                             => ( Tokens.LT (!lineRef, !lineRef) );
<INITIAL> ">="                            => ( Tokens.GEQ (!lineRef, !lineRef) );
<INITIAL> "<="                            => ( Tokens.LEQ (!lineRef, !lineRef) );
<INITIAL> "&"                             => ( Tokens.AND (!lineRef, !lineRef) );
<INITIAL> "|"                             => ( Tokens.OR (!lineRef, !lineRef) );
<INITIAL> "("                             => ( Tokens.LPAREN (!lineRef, !lineRef) );
<INITIAL> ")"                             => ( Tokens.RPAREN (!lineRef, !lineRef) );
<INITIAL> "["                             => ( Tokens.LSQBRACK (!lineRef, !lineRef) );
<INITIAL> "]"                             => ( Tokens.RSQBRACK (!lineRef, !lineRef) );
<INITIAL> "{"                             => ( Tokens.LFLOBRACK (!lineRef, !lineRef) );
<INITIAL> "}"                             => ( Tokens.RFLOBRACK (!lineRef, !lineRef) );
<INITIAL> "nil"                           => ( Tokens.NIL (!lineRef, !lineRef) );
<INITIAL> ":"                             => ( Tokens.COLON (!lineRef, !lineRef) );
<INITIAL> ":="                            => ( Tokens.ASSIGN (!lineRef, !lineRef) );
<INITIAL> ","                             => ( Tokens.COMMA (!lineRef, !lineRef) );
<INITIAL> "."                             => ( Tokens.DOT (!lineRef, !lineRef) );
<INITIAL> ";"                             => ( Tokens.SEMICOLON (!lineRef, !lineRef) );
<INITIAL> {digit}+                        => ( Tokens.INT (toInt yytext, !lineRef, !lineRef) );
<INITIAL> {legal_vars}                    => ( Tokens.ID (yytext, !lineRef, !lineRef) ); 