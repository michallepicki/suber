Nonterminals program_p top_level_item_p expr_p let_p fun_p if_then_else_p call_p subexpr_p field_access_p parens_p record_p record_fields_p const_p variable_p.
Terminals 'let' 'rec' 'in' 'fun' 'if' 'then' 'else' 'identifier' 'number' '(' ')' '{' '}' '.' '->' '=' ';'.
Rootsymbol program_p.

variable_p -> 'identifier' : {'variable_expr', extract_value('$1')}.

const_p -> 'number' : {'literal_int_expr', extract_value('$1')}.

record_fields_p -> 'identifier' '=' expr_p ';' record_fields_p : [{extract_value('$1'), '$3'} | '$5'].
record_fields_p -> 'identifier' '=' expr_p : [{extract_value('$1'), '$3'}].

record_p -> '{' record_fields_p '}' : {'record_expr', '$2'}.
record_p -> '{' '}' : {'record_expr', []}.

parens_p -> '(' expr_p ')' : '$2'.

field_access_p -> subexpr_p '.' 'identifier' : {'field_access_expr', '$1', extract_value('$3')}.

subexpr_p -> field_access_p : '$1'.
subexpr_p -> parens_p : '$1'.
subexpr_p -> record_p : '$1'.
subexpr_p -> const_p : '$1'.
subexpr_p -> variable_p : '$1'.

call_p -> subexpr_p : '$1'.
call_p -> call_p subexpr_p : {call_expr, '$1', '$2'}.

if_then_else_p -> 'if' expr_p 'then' expr_p 'else' expr_p : {'call_expr', {'call_expr', {'call_expr', {'variable_expr', "if"}, '$2'}, '$4'}, '$6'}.

fun_p -> 'fun' 'identifier' '->' expr_p : {'fun_def_expr', extract_value('$2'), '$4'}.

let_p -> 'let' 'rec' 'identifier' '=' expr_p 'in' expr_p : {'let_expr', 'true', extract_value('$3'), '$5', '$7'}.
let_p -> 'let'       'identifier' '=' expr_p 'in' expr_p : {'let_expr', 'false', extract_value('$2'), '$4', '$6'}.

expr_p -> let_p : '$1'.
expr_p -> fun_p : '$1'.
expr_p -> if_then_else_p : '$1'.
expr_p -> call_p : '$1'.

top_level_item_p -> 'let' 'rec' 'identifier' '=' expr_p : {'top_level_let_expr', 'true', extract_value('$3'), '$5'}.
top_level_item_p -> 'let'       'identifier' '=' expr_p : {'top_level_let_expr', 'false', extract_value('$2'), '$4'}.

program_p -> top_level_item_p program_p : ['$1' | '$2'].
program_p -> top_level_item_p : ['$1'].

Erlang code.

extract_value({_Token, _Line, Value}) -> Value.
