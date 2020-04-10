#lang brag
model : NEWLINE* (rule NEWLINE*)*
rule : head CAUSES body
head: table LEFT-BRACKET variable RIGHT-BRACKET
body: table LEFT-BRACKET variable RIGHT-BRACKET
table: ID
variable: ID