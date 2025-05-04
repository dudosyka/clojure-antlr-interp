grammar Grammar;

prog:   (expr|defn)*
    ;


item:   INT     # int
    |   STR     # str
    |   BOOL    # bool
    |   ID      # id
    ;

list:   '(' symbol=(RECUR|PRINT|ADD|ADDSTR|SUB|MUL|DIV|EQ|AND|OR|GR|GREQ|LESS|LESSEQ|ID) expr (NEWLINE? expr)* ')'
    ;

not_list: '(' NOT expr ')'
    ;

binding: ID (item|list);

bindings: binding (NEWLINE binding)*;

let: '(' LET '[' bindings ']' NEWLINE? block ')'
   ;

loop: '(' LOOP '[' bindings ']' NEWLINE? block ')'
    ;

if: '(' IF cond=expr NEWLINE? if_branch=expr NEWLINE? else_branch=expr ')'
  ;

defn: '(' DEFN ID '[' ID* ']' NEWLINE? block ')'
    ;

expr:   list
    |   not_list
    |   let
    |   loop
    |   if
    |   item
    |   NEWLINE
    ;

block: expr+;

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

ID  :   [a-zA-Z_-]+ ;
NEWLINE:'\r'? '\n' ;
WS  :   [ \t]+ -> skip ;
