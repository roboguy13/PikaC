# Immediate Goals

1. Design and implement a proper type system for layout polymorphism for Pika.
2. Make an intermediate language, PikaCore, that is about halfway between Pika
   and SSL. This layout-monomorphic language still has a functional, expression-based syntax,
   but all data structures are manipulated directly as SSL assertions. Give this intermediate language an exact specification. PikaCore could be used to
   translate to a SuSLik specification, an abstract machine semantics or generate C code
   directly. It should also simplify and clarify the design of the Pika
   translator.

The design and specification for these things are given in the `paper`
directory.

