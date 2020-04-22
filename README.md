carl-lib
========
[![Build Status](https://travis-ci.com/mkyl/carl-lib.svg?branch=master)](https://travis-ci.com/mkyl/carl-lib)

Causal Relational Learning library, implementation of the [corresponding paper](https://arxiv.org/abs/2004.03644).

**This software is currently in pre-alpha.**

Collection structure is as follows:

```
 | carl-lib/
  \
  | general:
  |- test/: unit and integration tests 
  |- scribblings/: documentation source
  | 
  | submodules:
  |- lang/: parsing language
  |- grounding/: creating ground causal model (GCM) from database instance
  |- embedding/: augmentation of GCM using embeddings
  |- detection/: covariate detection
  |- unit-table/: constructing the unit table
  |- estmation/: estimating causal quantities
  .
```
