# Memory Management
Musings on dynamic memory management in `Khaki`.

## Necessary Heap Allocations
`Khaki`'s performance philosophy includes maintaining an absolute minimum of heap allocations.
Most functional programming languages have prolific heap allocations.
`OCaml` and `Haskell` both have a uniform runtime representation, which means that by default, any complex types like tuples, closures, custom types, and (in `OCaml`'s case at least) floats need to be heap allocated.
Although most people don't consider Rust to be a functional programming language, the feature set it supports overlaps the features of many functional programming languages.
Despite Rust's reputation as an extremely performant language, there are still cases where `Rust` developers are pushed towards heap allocations where similar programs written in `Khaki` would be allocation-free (`Box<dyn Fn>` vs `Khaki`'s defunctionalization)[^1].

In particular, `Khaki` avoids heap allocations for:
- compound types like enums and tuples,
- closures
- floats (when they are supported :))

It is able to do this when traditional functional programming languages were not because of aggressive monomorphization during compilation, similar to the compilation strategy of `C++`.
Unfortunately, it is not practical to entirely eliminate heap allocations in a functional programming language.

## Recursive Data Structures
Functional programming languages frequently have a linked list data type defined like so:
```
enum List[a] {
  cons(<|a, List[a]|>),
  nil(<||>)
}
```
`Khaki` would compile a `List[Int]::cons` cell to `c` code that looks something like this[^2]
```
struct List;
struct Cons {
  int field0;
  List field1;
};
struct List {
  bool is_nil;
  Cons cons;
};
```
Which won't compile in `c` because you can't have a direct reference to a `List` before the `List` struct is defined.
This makes sense because it means in order to have enough stack space to allocate a `List`, you need enough stack space for a `Cons`, but for that you need enough for a `List`, and so on to infinity.
The way that `c` programmers normally address this issue when writing linked lists is either to:
1. Not use linked lists.
2. Change the `Cons cons` field of `List` to `Cons *cons`, and dynamically allocate each new `Cons` cell using `malloc`.

Although option 1 does make a good point that it might be worthwhile to have a built-in performant list data structure other than a linked list, we still want to support recursive data types.
This means that in order to support recursive data structures, `Khaki` needs dynamic memory allocation.

## Khaki's Approach to Dynamic Memory Allocation
There are several different strategies that `Khaki` could use to implement dynamic memory allocation.

### Manual Memory Management
Whenever users want to introduce an indirection, they could wrap their data using a function that looks like `fn malloc[t](x: t) -> Ptr[t]`, and when they are done with their data, they could call a function like `fn free[t](x: Ptr[t])`.
This strategy would be extremely simple to implement but it would be error-prone.
While use-after-free bugs could be avoided using a technique like [generational references](https://verdagon.dev/blog/generational-references), users would be forced to write lengthy routines to deallocate memory, which would be easy to forget, leading to a memory leak.

### Tracing Garbage Collection
Traditional tracing garbage collection is a promising option.
In addition to its famous users, including `Java` and `Go`, tracing garbage collection is frequently used by functional programming languages, such as `Common Lisp`, `Haskell`, and `OCaml`.
Tracing garbage collection is a technique where the runtime periodically walks the call stack to flag pieces of memory that are still in use, then walks the heap while freeing blocks of memory that weren't tagged as in use.
While this sounds computationally intensive, modern garbage collectors have remarkably good performance.
Unfortunately, garbage collection is complicated by a lack of a uniform representation: we would need to add additional metadata to each heap-allocated object for the garbage collector to work.

### Reference Counting
Reference counting is also a promising option.
While few languages use exclusively reference counting for memory management, Swift has shown that it is tractable, and Rust uses it extensively when memory management is necessary.
The central idea is that each heap allocation is wrapped in an `Rc[t]`, which is created by a `fn malloc[t](x: t) -> Rc[t]`.
Unlike manual memory management, instead of users having to manually call `free`, each `Rc` keeps track of the number of references to it internally, with the compiler adding calls to `fn increment_refs[t](x: Rc[t])` and `fn decrement_refs[t](x: Rc[t])` when necessary.
When the reference count of an `Rc` drops to 0, the memory it points to is deallocated.

This approach has the benefit of being simple for users: it is impossible to get a use-after-free bug, and memory leaks are prevented in the absence of `Rc` cycles.
In addition, there have been a number of recent developments in reference counting, including [Lean's reference counter](https://arxiv.org/pdf/1908.05647) and [Koka's reference counter](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf), that make it possible to reduce the number of reference count instructions and reuse allocations.
For these reasons, Khaki uses reference counting to manage its memory.

## Unintrusive Dynamic Memory Allocation
While it would be simplest to force users to manually add `Rc`s wherever they deem necessary in recursive data structures, it is possible to have the compiler insert all the `Rc`s and not force the user to deal with the complexity of memory management.
The compiler needs to construct a directed graph of each enum, where there is an edge from one enum to another if the first enum contains the other.
Each cycle in this graph represents a recursive data structure, and by replacing some enum arguments with an `Rc` to the argument and cutting the corresponding edges, the compiler could eliminate all cycles.
While the problem of which edges to cut to insert the minimum number of `Rc`s is actually [NP-hard](https://en.wikipedia.org/wiki/Feedback_arc_set), a simple greedy algorithm would perform well enough to work for `Khaki`'s purposes.

To support this paradigm, no existing compiler passes would need to be changed. Instead, a `ref-count` pass could be inserted directly before `lower`ing to `base` that inserts `Rc`s wherever necessary and fixes existing code to work with them.

[^1]: To be clear, I am not saying Rust is a bad language here, just that it wasn't designed for seamless functional programming and that occaisonally shows
[^2]: Khaki's generated `c` is much less pretty than this, and there would be about 4 other important structs and enums in the generated code, but this example gets at the root of the problem.
