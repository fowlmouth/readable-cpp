ReadableCpp is a partial implementation of the [SPECS proposal](http://www.csse.monash.edu.au/~damian/papers/HTML/ModestProposal.html). It is an alternate syntax for C++. No semantics are enforced, no runtime is used, the syntax should map 1:1 with C++.

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
double (*(*foo)(int ))(float );
```
### English types
You can also use a limited form of English to declare variables, doesn't that sound fun? I'm still working on this and there are some kinks to work out.

* ^ is ('pointer to' || 'ptr to' || 'ptr')
* & is ('reference to' || 'ref to') 
* [] is 'array of' (unsized array)
    * 'array of 5' || 'array of (CONST_EXPR)' for a sized array
* 'function taking (ARGUMENTS) returning'

```c++
var x is a pointer to array of 5 int,
    y is an array of (someconstant) pointer to function taking int, []*const char returning void
//C++
int (*x)[5];
void (*y[someconstant])(int , const char *[]);
```
### Include
Does what you expect it to. If you include a file with the suffix "lhh" it will look for an LPP file by that name and instruct it to build headers also.
```c++
include <iostream>  //semicolon or EOL expected
include "some.lpp", "someother.lpp"
//output
#include <iostream>
#include "some.hpp"
#include "someother.hpp"
```
### Import
Import statement looks for a package file. Package is a simple YAML file that includes information for the linker (oh btw a simple build script is generated also)
```c++
import SFML/Graphics
```
### Var declaration
```c++
var foo, bar: ^int
var zz: static int
//C++
int (*foo), (*bar);
static int zz;

//Three kinds of declaration are supported:
var foo: * const* const int, //simple
    bar(0), baz(42): int,    // initializers
    callback: *(&obj, *const char)->int = &someFunc // assignment

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
```

#### More Information
* C type decl decoder http://cdecl.org/
* Reading C type declarations http://unixwiz.net/techtips/reading-cdecl.html
* SPECS Proposal http://www.csse.monash.edu.au/~damian/papers/HTML/ModestProposal.html
