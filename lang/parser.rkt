#lang brag
model : NEWLINE* (line NEWLINE*)*
line : rule | query
query : head CAUSES head QUESTION-MARK
rule : head CAUSES body
head: table LEFT-BRACKET variable RIGHT-BRACKET
body: table LEFT-BRACKET variable RIGHT-BRACKET
table: ID
variable: ID