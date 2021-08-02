# suber

Basic Erlang implementation of the Simple-sub typechecking algorithm for Algebraic Subtyping described by [Lionel Parreaux](https://github.com/LPTK) in his [ICFP Pearl paper](https://infoscience.epfl.ch/record/278576), and the basis of the accompanying programming language (no codegen yet). The original, more complete Scala implementation is available [on GitHub](https://github.com/LPTK/simple-sub)

To compile and run the type checker on an example `test_file.suber`, run
```
escript -c suber.erl
```

Modify the `test_file.suber` to make a typo in a record field name, re-run and you should get an error!
