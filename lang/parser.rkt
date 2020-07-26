#lang brag
model : NEWLINE* (line NEWLINE*)*
line : rule1 | query
rule1 : predicate CAUSES predicate (WHERE predicate-list)?
predicate-list: predicate (COMMA predicate)*
query : rule1 QUESTION-MARK
predicate: symbol LEFT-BRACKET symbol-list RIGHT-BRACKET
symbol-list: symbol (COMMA symbol)*
symbol: ID
