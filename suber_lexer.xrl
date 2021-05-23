Definitions.

IDENT      = [a-z_]+
TAG        = `[A-Z][a-z_]+
WHITESPACE = [\s\t\n\r]

Rules.

let           : {'token', {'let', TokenLine}}.
rec           : {'token', {'rec', TokenLine}}.
and           : {'token', {'and', TokenLine}}.
fun           : {'token', {'fun', TokenLine}}.
->            : {'token', {'->', TokenLine}}.
=             : {'token', {'=', TokenLine}}.
true          : {'token', {'true', TokenLine}}.
false         : {'token', {'false', TokenLine}}.
if            : {'token', {'if', TokenLine}}.
then          : {'token', {'then', TokenLine}}.
else          : {'token', {'else', TokenLine}}.
match         : {'token', {'match', TokenLine}}.
with          : {'token', {'with', TokenLine}}.
in            : {'token', {'in', TokenLine}}.
\|            : {'token', {'|', TokenLine}}.
{             : {'token', {'{',  TokenLine}}.
}             : {'token', {'}',  TokenLine}}.
\(            : {'token', {'(',  TokenLine}}.
\)            : {'token', {')',  TokenLine}}.
;             : {'token', {';',  TokenLine}}.
\.            : {'token', {'.',  TokenLine}}.
{IDENT}       : {'token', {'ident', TokenLine, TokenChars}}.
{TAG}         : {'token', {'tag', TokenLine, TokenChars}}.
{WHITESPACE}+ : 'skip_token'.

Erlang code.
