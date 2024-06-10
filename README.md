# Jink 
The Jink programming language

[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/jink-lang/jink/rust.yml?style=for-the-badge&label=Tests)](https://github.com/jink-lang/jink/actions/workflows/rust.yml)
![GitHub last commit](https://img.shields.io/github/last-commit/jink-lang/jink?style=for-the-badge)
[![GitHub contributors](https://img.shields.io/github/contributors-anon/jink-lang/jink?style=for-the-badge)](https://github.com/jink-lang/jink/graphs/contributors)
[![Discord](https://img.shields.io/discord/365599795886161941?label=Discord&style=for-the-badge)](https://discord.gg/cWzcQz2)
[![License](https://img.shields.io/github/license/jink-lang/jink?style=for-the-badge)](LICENSE)
![GitHub Repo stars](https://img.shields.io/github/stars/jink-lang/jink?style=for-the-badge)

## About
This is the home of the [Jink](https://github.com/jink-lang/jink) programming language. This is a WIP and any contributions are welcome. Read more in the [Contributing](#contributing) section below.

There is a [Discord server](https://discord.gg/cWzcQz2) that hosts the project where you can ask questions, get help, and chat with other contributors.

## Status
The project is in its very early stages and being actively developed. If you have familiarity with compilers and/or LLVM, your expertise would be greatly appreciated.

We have begun implementing the first stage compiler in Rust. The interpreter exists solely to mess about with the language and to test/simulate it. There are a number of issues and features that need to be addressed before we can move on to the next stage.

#### Goal Checklist / TODOs

The lexer and parser are now up to snuff with and surpass the original interpreted implementation. Here are a list of goals that scope out the new project and repository, not in or by any particular order or specification.

##### General
- [ ] Design standard for modules
- [ ] Start writing standard library
- [ ] Start writing self-hosted compiler
- [ ] Add examples
- [ ] Add documentation / tutorials
- [ ] Add issue templates
- [ ] Add pull request templates
##### Compiler
- [ ] Standardize type inference
- [ ] Fix nested conditionals
- [ ] Build dynamic array type
- [ ] Build heterogenous array type
- [ ] Build nested arrays
- [x] Build structs/types
- [x] Build nested structs
- [x] Build struct indexing
- [ ] Build nested struct indexing
- [x] Build for loops
- [x] Build while loops
- [x] Build nested loops
- [ ] Build break and continue for loops
- [x] Build module/import system
- [ ] Validate module imports
- [ ] Handle named module indexing
- [ ] Build string interpolation
- [ ] Write loop tests
- [ ] Write struct tests
- [ ] Write function tests
- [ ] Write module tests
##### Lexer
- [ ] Lex string interpolation
##### Parser
- [x] Parse type definitions
- [x] Parse objects
- [x] Parse arrays
- [x] Parse classes
- [ ] Parse self keyword in classes
- [ ] Parse self keyword in methods
- [x] Parse module imports
- [ ] Validate against circularly imported modules
- [ ] Parse string interpolation
- [x] Parse indexing (arrays)
- [ ] Parse recursive indexing (arrays)
- [x] Parse indexing (object properties and methods)
- [x] Parse recursive indexing (object properties and methods)
- [ ] Parse del statements
- [x] Parse while loops
- [x] Parse for loops
- [x] Parse break and continue for loops
- [x] Parse public statements
- [ ] Parse external statements

## Examples

##### Hello World
```js
print("Hello, World!"); // Hello, World!
```

##### Variables
```js
const name = "Jacob";
let age;
age = 1 + 100 / 4;

print(name); // Jacob
print(age); // 26
```

##### Functions
```js
// Inline
fun inline(let abc) return abc;
fun inline_newline(let def)
  return def;

// Typed params & return types
fun add(int a, int b) -> int {
  return a + b;
}

// Recursion (Fibonacci sequencer)
fun fib(let number) {
  if (number <= 1) return number
  return fib(number - 2) + fib(number - 1)
}

print(fib(10)) // 55

// Dynamic defaults
fun test(let a: 5) {
  print(a);
}

test() // 5
```

##### Modules
```js
import module
import module as hi
import module.*
import module.abc.*
import module.abc.def
import module.abc.def as xyz
import from module.abc { def as xyz, mno, ghi as jkl }
import from module.abc.def {
  ghi as jkl,
  mno,
  pqr as stu
}
```

##### Classes
```js
type address = {
  street: string,
  city: string,
  state: string,
  zip: int
};

pub cls Business = {
  fun new(string name, address addr) -> self {
    self.name = name;
    self.address = addr;
  };

  pub fun get_address() -> address {
    return self.address;
  };
};

const business = Business("Jink", {
  street: "1234 Jink St.",
  city: "Jinkville",
  state: "Jinkland",
  zip: 12345
});

print(business.get_address().city); // Jinkville
```

## Installation

### Prerequisites

You will need to have Rust installed on your machine. You can follow the instructions on the [Rust website](https://www.rust-lang.org/tools/install) to install it.

You will also need (at least) LLVM 16.0.0 installed on your machine. You can follow the instructions on the [LLVM website](https://releases.llvm.org/download.html) to install it.

### Running

To run the project, you can run the following command:

```bash
cargo run ./path/to/file.jk
```

To run the compiler in verbose mode, you can run the following command:

```bash
cargo run ./path/to/file.jk -v
```

This will help you see the output of the parser and IR builder and debug any issues you may have.

To run the interpreter, you can run the following command:

```bash
cargo run ./path/to/file.jk -i
```

## Contributing

Contributions are very welcome. Please read the [CONTRIBUTING.md](./.github/docs/CONTRIBUTING.md) file to get acquainted with the process and guidelines. If you have any questions, feel free to ask in Jacob's project [Discord server](https://discord.gg/cWzcQz2).

## License

This project is distributed under the GPLv3 License - see the [license file](LICENSE) for details.

Copyright © 2024 Jacob Noah, jink-lang contributors
