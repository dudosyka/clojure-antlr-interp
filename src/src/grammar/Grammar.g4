grammar Grammar;

prog:   (expr|defn|NEWLINE)*
    ;


item:   INT     # int
    |   '-' INT # negativeInt
    |   STR     # str
    |   BOOL    # bool
    |   ID      # id
    ;

list:   '(' symbol=(RECUR|PRINTLN|PRINT|ADD|ADDSTR|SUB|MUL|DIV|EQ|AND|OR|GR|GREQ|LESS|LESSEQ|ID) expr (NEWLINE? expr)* ')'
    ;

not_list: '(' NOT expr ')'
    ;

binding: ID expr;

bindings: binding (NEWLINE binding)*;

let: '(' LET '[' bindings ']' NEWLINE? block ')'
   ;

loop: '(' LOOP '[' bindings ']' NEWLINE? block ')'
    ;

if: '(' IF cond=expr NEWLINE? if_branch=expr NEWLINE? else_branch=expr ')'
  ;

arg: type=(STR_TYPE|INT_TYPE|BOOL_TYPE) ID
   ;

defn: '(' DEFN ID '[' arg* ']' block ')'
    ;

expr:   list
    |   not_list
    |   let
    |   loop
    |   if
    |   item
    ;

block: (NEWLINE? expr)+;

//KEYWORD
LOOP   : 'loop'   ;
IF     :  'if'    ;
LET    : 'let'    ;
RECUR  : 'recur'  ;
PRINT  : 'print'  ;
PRINTLN: 'println';
DEFN   : 'defn'   ;


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


INT :   [0-9]+ ;
BOOL:   'true'|'false';
STR :   '"' (~'"' | '\\' '"')* '"' ;


STR_TYPE:  '^str' ;
INT_TYPE:  '^int' ;
BOOL_TYPE: '^bool';

ID  :   [a-zA-Z_-]+ ;
NEWLINE:'\r'? '\n' ;
WS  :   [ \t]+ -> skip ;
