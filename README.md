# suber

Basic Erlang implementation of the CubiML Algebraic Subtyping typechecking algorithm described by [Robert Grosse](https://github.com/Storyyeller) in his [blog post series](https://blog.polybdenum.com/2020/07/04/subtype-inference-by-example-part-1-introducing-cubiml.html). The original, more complete Rust implementation is available [on GitHub](https://github.com/Storyyeller/cubiml-demo)

To compile and run the type checker on an example `test_file.suber`, run
```
escript -c suber.erl
```

Does not implement "Let polymorphism" (yet?)
