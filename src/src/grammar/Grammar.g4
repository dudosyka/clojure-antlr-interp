grammar Grammar;

prog:   expr*;

item:   INT     # int
    |   STR     # str
    |   BOOL    # bool
    |   ID      # id
    ;

list:   '(' symbol=(RECUR|PRINT|ADD|ADDSTR|SUB|MUL|DIV|EQ|AND|OR|NOT|GR|GREQ|LESS|LESSEQ) expr+ ')'
    ;

binding: ID (item|list);

bindings: binding (NEWLINE binding)*;

let: '(' LET '[' bindings ']' NEWLINE? block ')'
   ;

loop: '(' LOOP '[' bindings ']' NEWLINE? block ')'
    ;

if: '(' IF cond=expr NEWLINE? if_branch=expr NEWLINE? else_branch=expr ')'
  ;

//defn: '(' DEFN '[' ID* ']' NEWLINE? expr ')'
//    ;

expr:   list
    |   let
    |   loop
    |   if
    |   item
//    |   defn
    |   NEWLINE
    ;

block: expr+;

//intExpr:    LPAREN op=(ADD|SUB|MUL|DIV|EQ) intExpr* RPAREN      # intOp
//       |    INT                                                 # int
//       |    ID                                                  # intId
//       ;
//
//strExpr:    LPAREN op=(ADD|EQ) strExpr* RPAREN                  # strOp
//       |    STR                                                 # str
//       |    ID                                                  # strId
//       ;
//
//boolExpr:   op=(AND|OR) boolExpr*               # boolOp
//        |   BOOL                                # bool
//        |   ID                                  # boolId
//        ;
//
//bindings:     ID WS expr (NEWLINE ID WS expr)*    # binding
//        ;
//
//recur:  LPAREN RECUR expr (WS expr)* RPAREN
//     ;
//
//let:  LET LSBRACE bindings RSBRACE expr*
//   ;
//
//loop:   LET LSBRACE bindings RSBRACE (expr|RECUR)*
//    ;
//
//expr:   intExpr                                 # intE
//    |   boolExpr                                # boolE
//    |   strExpr                                 # strE
//    |   PRINT expr (WS expr)*                   # print
//    ;

//stat:   (numExpr|strExpr|boolExpr) NEWLINE      # printExpr
//    |   ID ASSIGN numExpr  NEWLINE              # assignInt
//    |   ID ASSIGN strExpr  NEWLINE              # assignStr
//    |   ID ASSIGN boolExpr NEWLINE              # assignBool
//    |   while                                   # whileLoop
//    |   NEWLINE                                 # blank
//    ;
//
//numExpr:    numExpr op=(MUL|DIV) numExpr        # MulDiv
//       |    numExpr op=(ADD|SUB) numExpr        # AddSub
//       |    INT                                 # int
//       |    ID                                  # numId
//       |    LPAREN numExpr RPAREN               # parens
//       ;
//
//strExpr:    strExpr (ADD|MUL) numExpr           # AddMulStr
//       |    STR                                 # str
//       |    ID                                  # strId
//       |    LPAREN strExpr RPAREN               # strParens
//       ;
//
//boolExpr:   boolExpr op=(AND|OR) boolExpr       # AndOr
//        |   BOOL                                # bool
//        |   numExpr                             # numToBool
//        |   strExpr                             # strToBool
//        |   ID                                  # boolId
//        |   LPAREN boolExpr RPAREN              # boolParens
//        ;
//
//while:      WHILE boolExpr stat                 # whileSingleLine
//     |      WHILE boolExpr LBRACE stat+ RBRACE  # whileMultiLine
//     ;
//


//KEYWORD
LOOP : 'loop' ;
IF   :  'if'  ;
LET  : 'let'  ;
RECUR: 'recur';
PRINT: 'print';
DEFN : 'defn' ;

// OPERATORS
MUL :   '*'   ;
DIV :   '/'   ;
ADD :   '+'   ;
ADDSTR: 'str' ;
SUB :   '-'   ;
EQ:     '='   ;
GR  :   '>'   ;
GREQ:   '>='  ;
LESS:   '<'   ;
LESSEQ: '<='  ;
AND :   'and' ;
OR  :   'or'  ;
NOT :   'not' ;


// DATA TYPES
INT :   [0-9]+ ;
BOOL:   'true'|'false';
STR :   '"' (~'"' | '\\' '"')* '"' ;


LPAREN : '(' ;
RPAREN : ')' ;
LBRACE : '{' ;
RBRACE : '}' ;
LSBRACE: '[' ;
RSBRACE: ']' ;
ID  :   [a-zA-Z]+ ;
NEWLINE:'\r'? '\n' ;
WS  :   [ \t]+ -> skip ;
