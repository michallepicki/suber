Definitions.

IDENTIFIER = [a-z_]+[0-9]?
WHITESPACE = [\s\t\n\r]
NUMBER     = [0-9]+

Rules.

let           : {'token', {'let', TokenLine}}.
rec           : {'token', {'rec', TokenLine}}.
in            : {'token', {'in', TokenLine}}.
fun           : {'token', {'fun', TokenLine}}.
if            : {'token', {'if', TokenLine}}.
then          : {'token', {'then', TokenLine}}.
else          : {'token', {'else', TokenLine}}.
{IDENTIFIER}  : {'token', {'identifier', TokenLine, TokenChars}}.
{NUMBER}      : {'token', {'number', TokenLine, list_to_integer(TokenChars)}}.
\(            : {'token', {'(',  TokenLine}}.
\)            : {'token', {')',  TokenLine}}.
{             : {'token', {'{',  TokenLine}}.
}             : {'token', {'}',  TokenLine}}.
\.            : {'token', {'.',  TokenLine}}.
->            : {'token', {'->', TokenLine}}.
=             : {'token', {'=', TokenLine}}.
;             : {'token', {';', TokenLine}}.
{WHITESPACE}+ : 'skip_token'.

Erlang code.
