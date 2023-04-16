# Immediate Goals

1. Design and implement a *proper* system of layout polymorphism, including a type
   system, for Pika.
2. Make an intermediate language, PikaCore, that is about halfway between Pika
   and SSL. This layout-monomorphic language still has a functional, expression-based syntax,
   but all data structures are manipulated directly as SSL assertions. Give this intermediate language an exact specification. PikaCore could be used to
   translate to a SuSLik specification, an abstract machine semantics or generate C code
   directly. It should also simplify and clarify the design of the Pika
   translator.
3. Equations in Pika are currently given in the style of a language like
   Haskell, with pattern matching on the left-hand side and arbitrary
   (well-scoped) expressions on the right-hand side. Can this be generalized
   to more kinds of equations (maybe even arbitrary equations)?
   **Key idea**: In the translated version of Pika code, the input and
   output are not really distinguished from each other. They are both
   given as part of one SSL assertion. This seems like we should be
   able to translate Pika equations that have arbitrary expressions on
   either side of the equals sign.

The design and specification for these things are given in the `paper`
directory.

