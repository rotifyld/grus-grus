# Grus grus

**Grus grus** is a functional programming language with interpreter written in Haskell. 

*Grus grus* is a taxonomic name of the *common crane* bird with *grus* meaning *crane* in Latin.

## Main features
 - functional
 - static type check
 - algebraic data types
 - non-deterministic program flow
 
## Example program

More examples available in *examples/* folder with *examples/bad/* showing various error messages and *examples/good/* some working programs. 

This example is available as `examples/good/permutations.gg`:

```
alg List = Nil | Cons(Int, List);

fun insert(el: Int, l: List) -> List {
    case l of {
        _ ~> Cons(el, l);
        Cons(head, tail) ~> Cons(head, insert(el, tail));
    }
}

fun permutation(l: List) -> List {
    case l of {
        Cons(x, Nil) ~> l;
        Cons(head, tail) ~> insert(head, permutation(tail));
    }
}

permutation(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
```

and returns:

```
Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
Cons(2, Cons(1, Cons(3, Cons(4, Nil))))
Cons(2, Cons(3, Cons(1, Cons(4, Nil))))
Cons(2, Cons(3, Cons(4, Cons(1, Nil))))
Cons(1, Cons(3, Cons(2, Cons(4, Nil))))
Cons(3, Cons(1, Cons(2, Cons(4, Nil))))
Cons(3, Cons(2, Cons(1, Cons(4, Nil))))
Cons(3, Cons(2, Cons(4, Cons(1, Nil))))
Cons(1, Cons(3, Cons(4, Cons(2, Nil))))
Cons(3, Cons(1, Cons(4, Cons(2, Nil))))
Cons(3, Cons(4, Cons(1, Cons(2, Nil))))
Cons(3, Cons(4, Cons(2, Cons(1, Nil))))
Cons(1, Cons(2, Cons(4, Cons(3, Nil))))
Cons(2, Cons(1, Cons(4, Cons(3, Nil))))
Cons(2, Cons(4, Cons(1, Cons(3, Nil))))
Cons(2, Cons(4, Cons(3, Cons(1, Nil))))
Cons(1, Cons(4, Cons(2, Cons(3, Nil))))
Cons(4, Cons(1, Cons(2, Cons(3, Nil))))
Cons(4, Cons(2, Cons(1, Cons(3, Nil))))
Cons(4, Cons(2, Cons(3, Cons(1, Nil))))
Cons(1, Cons(4, Cons(3, Cons(2, Nil))))
Cons(4, Cons(1, Cons(3, Cons(2, Nil))))
Cons(4, Cons(3, Cons(1, Cons(2, Nil))))
Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
```

Which is a list of all possible permutations of list `[1, 2, 3, 4]`. (Note that these is no syntactic sugar available)

### Boiling it down:

```
alg List = Nil | Cons(Int, List);
```

Declares algebraic data type `List` consisting of two constructors: constant `Nil`, and `Cons` which takes two arguments: one of type `Int` and one of type `List` (recursive definition).  

```
fun insert(el: Int, l: List) -> List {
    ...
}
```

Function declaration of type `Int -> List -> List`. Note that partial application is permitted, e.g. expression `insert(1)` is valid, and is of type `List -> List`.

```
case l of {
    _ ~> Cons(el, l);
    Cons(head, tail) ~> Cons(head, insert(el, tail));
}
```

Case expression that:
 - for all elements returns list with prepended `el`.
 - for elements of shape `Cons(_, _)` (non-empty lists) returns list that is constructed with a recursive call to the function `insert`.
 
Note that non-empty list fall into both of these categories and so **the execution branches** and both of this expressions are evaluated and returned. In fact function `insert` returns not simply a list, but a set of lists. This is how the non-determinism is included in the language.
 
If we were to call expression `insert(4, Cons(1, Cons(2, Cons(3, Nil))))` we'd get result:

```
Cons(4, Cons(1, Cons(2, Cons(3, Nil))))
Cons(1, Cons(4, Cons(2, Cons(3, Nil))))
Cons(1, Cons(2, Cons(4, Cons(3, Nil))))
Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
```
