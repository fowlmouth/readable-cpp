LazyPP is a partial implementation of the [SPECS proposal](http://www.csse.monash.edu.au/~damian/papers/HTML/ModestProposal.html). It is an alternate syntax for C++. No semantics are enforced, no runtime is used, the syntax should map 1:1 with C++.

### Type declaration syntax
This is the most important difference from C++. Studies have shown that reading type declarations in C can cause dizziness, drymouth, nausea, diabetes, ocular warts and constipation and may be linked to low sperm count[[citation needed]](http://google.com). They would have you believe that you should look at the name, go right until you cant, go left until you cant and repeat until you have the whole type, I believe that we should read it left-to-right, like English (God's language.)
```c++
var foo: ^ (void) -> int;
foo = &zoo;
cout << foo! << endl;
//equiv C++
int (*foo)(void);
foo = &zoo;
cout << foo() << endl;
//not very exciting, here's another example
var foo: ^ (int) -> ^ (float) -> double;
//reads like english, pointer to a function (int) returning a pointer to a function (float) returning double
//here's the horror it compiles to
double ((*((*foo)(int )))(float ));
```
### Var declaration
```c++
var foo, bar: ^int;
//C++
int (*foo), (*bar);
```
### Function declaration
```c++
func main : (argc: int, argv: []^char) -> int {
  for var i(0): int; i < argc; i++ {
    std::cout << argv[i] << endl;
  }
  return 0;
}
```
### Switch Statement
```c++
switch x {
  case 0, 1, 2: cout << "zero o uno o dos";
  case 3: cout << "three";
}
//compiles to
switch(x) {
  case 0:
  case 1:
  case 2:
    cout << "zero o uno o dos";
    break;
  case 3:
    cout << "three";
    break;
}

#### More Information
* C type decl decoder http://cdecl.org/
* Reading C type declarations http://unixwiz.net/techtips/reading-cdecl.html