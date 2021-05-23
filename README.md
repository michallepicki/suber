# suber

Basic Erlang implementation of the Cubic Biunification typechecking algorithm for Algebraic Subtyping described by [Robert Grosse](https://github.com/Storyyeller) in his [blog post series](https://blog.polybdenum.com/2020/07/04/subtype-inference-by-example-part-1-introducing-cubiml.html), and the basis of the accompanying CubiML programming language (no codegen yet). The original, more complete Rust implementation is available [on GitHub](https://github.com/Storyyeller/cubiml-demo)

To compile and run the type checker on an example `test_file.suber`, run
```
escript -c suber.erl
```

Modify the `test_file.suber` to make a typo in a record field name, or case tag, re-run and you should get an error!

Does not implement "Let polymorphism" (yet?)
