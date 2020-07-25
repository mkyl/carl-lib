#lang brag
model : NEWLINE* (line NEWLINE*)*
line : rule | query
rule : predicate CAUSES predicate (WHERE predicate-list)?
predicate-list: predicate (COMMA predicate)*
query : rule QUESTION-MARK
predicate: symbol LEFT-BRACKET symbol-list RIGHT-BRACKET
symbol-list: symbol (COMMA symbol)*
symbol: ID
