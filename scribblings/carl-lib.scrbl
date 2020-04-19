#lang scribble/manual
@require[@for-label[carl-lib
                    racket/base]]
@(require scribble/extract)

@title{carl-lib}
@author{Moe Kayali}

@defmodule[carl-lib]

The Causal Relational Learning (CaRL) framework is a tool for causal inference over relational data. 

This is the reference implementation of the framework developed in the paper ``@hyperlink["https://arxiv.org/abs/2004.03644"]{Causal Relational Learning}.''

@section{Language}

@defmodule[carl-lib/lang]
@include-extracted[carl-lib/lang]

@section{Grounding}

@defmodule[carl-lib/ground]
@include-extracted[carl-lib/ground]

@section{Embedding}

@defmodule[carl-lib/embed]
@include-extracted[carl-lib/embed]

@section{Covariate Detection}

@defmodule[carl-lib/detect]
@include-extracted[carl-lib/detect]

@section{Unit Table Construction}

@defmodule[carl-lib/unit-table]
@include-extracted[carl-lib/unit-table]

@section{Estimation}

@defmodule[carl-lib/estimate]
@include-extracted[carl-lib/estimate]
