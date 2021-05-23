Nonterminals module_def top_level_items top_level_item top_level_let_def top_level_let_rec_def expr match_expr match_arms match_arm call_or_case_or_simple_expr call_expr case_or_simple_expr simple_expr case_expr field_access_expr let_rec let_expr if_expr func_def_expr record_expr key_val_pairs literal_expr bool variable_expr.
Terminals 'let' 'rec' ident tag '=' 'true' 'false' 'in' 'and' ';' 'if' 'then' 'else' 'fun' '->' '{' '}' '(' ')' '.' 'match' 'with' '|'.
Rootsymbol module_def.

bool -> 'true' : {bool, 'true'}.
bool -> 'false' : {bool, 'false'}.

literal_expr -> bool : {literal_expr, '$1'}.

func_def_expr -> 'fun' ident '->' expr : {func_def_expr, extract_value('$2'), '$4'}.

if_expr -> 'if' expr 'then' expr 'else' expr : {if_expr, '$2', '$4', '$6'}.

let_expr -> 'let' ident '=' expr 'in' expr : {let_expr, {var_definition, extract_value('$2'), '$4'}, '$6'}.

let_rec -> 'let' 'rec' ident '=' expr 'and' let_rec : [{var_definition, extract_value('$3'), '$5'} | '$7'].
let_rec -> 'let' 'rec' ident '=' expr : [{var_definition, extract_value('$3'), '$5'}].

key_val_pairs -> ident '=' expr ';' key_val_pairs : [{extract_value('$1'), '$3'} | '$5'].
key_val_pairs -> ident '=' expr : [{extract_value('$1'), '$3'}].

record_expr -> '{' '}' : {record_expr, []}.
record_expr -> '{' key_val_pairs '}' : {record_expr, '$2'}.

field_access_expr -> simple_expr '.' ident : {field_access_expr, '$1', extract_value('$3')}.

variable_expr -> ident : {variable_expr, extract_value('$1')}.

simple_expr -> field_access_expr : '$1'.
simple_expr -> record_expr : '$1'.
simple_expr -> variable_expr : '$1'.
simple_expr -> literal_expr : '$1'.
simple_expr -> '(' expr ')' : '$2'.

case_expr -> tag expr : {case_expr, extract_value('$1'), '$2'}.

case_or_simple_expr -> simple_expr : '$1'.
case_or_simple_expr -> case_expr : '$1'.

call_expr -> call_or_case_or_simple_expr case_or_simple_expr : {call_expr, '$1', '$2'}.

call_or_case_or_simple_expr -> case_or_simple_expr : '$1'.
call_or_case_or_simple_expr -> call_expr : '$1'.

match_arm -> tag ident '->' call_or_case_or_simple_expr : {{extract_value('$1'), extract_value('$2')}, '$4'}.
match_arms -> '|' match_arm : ['$2'].
match_arms -> '|' match_arm match_arms : ['$2' | '$3'].
match_expr -> 'match' expr 'with' match_arms : {match_expr, '$2', '$4'}.

expr -> func_def_expr : '$1'.
expr -> if_expr : '$1'.
expr -> let_expr : '$1'.
expr -> match_expr : '$1'.
expr -> call_or_case_or_simple_expr : '$1'.

top_level_let_def -> 'let' ident '=' expr : {top_level_let_def, {var_definition, extract_value('$2'), '$4'}}.

top_level_let_rec_def -> let_rec : {top_level_let_rec_def, '$1'}.

top_level_item -> top_level_let_def : '$1'.
top_level_item -> top_level_let_rec_def : '$1'.

top_level_items -> top_level_item ';' top_level_items : ['$1' | '$3'].
top_level_items -> top_level_item ';' : ['$1'].

module_def -> top_level_items : '$1'.

Erlang code.

extract_value({_Token, _Line, Value}) -> Value.