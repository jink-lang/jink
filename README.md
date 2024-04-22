# Jink 
The Jink programming language

[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/jink-lang/jink/rust.yml?style=for-the-badge&label=Tests)](https://github.com/jink-lang/jink/actions/workflows/rust.yml)
![GitHub last commit](https://img.shields.io/github/last-commit/jink-lang/jink?style=for-the-badge)
[![GitHub contributors](https://img.shields.io/github/contributors-anon/jink-lang/jink?style=for-the-badge)](https://github.com/jink-lang/jink/graphs/contributors)
[![Discord](https://img.shields.io/discord/365599795886161941?label=Discord&style=for-the-badge)](https://discord.gg/cWzcQz2)
[![License](https://img.shields.io/github/license/jink-lang/jink?style=for-the-badge)](LICENSE)
![GitHub Repo stars](https://img.shields.io/github/stars/jink-lang/jink?style=for-the-badge)

## About
This is the home of the Rust compiler of the [Jink](https://github.com/jink-lang/jink) programming language. This is a WIP and any contributions are welcome. Read more in the [Contributing](#contributing) section below.

## Goal Checklist / TODOs

(Not in or by any particular order or specification)

##### General
- [x] Build lexer
- [x] Build parser
- [ ] Build optimizer
- [ ] Build bytecode/IR
- [ ] Build VM / compile steps
##### Lexer
- [ ] Lex string templates
##### Parser
- [x] Parse functions
- [x] Parse type definitions
- [x] Parse objects
- [x] Parse arrays
- [x] Parse classes
- [x] Parse function defaults
- [ ] Parse string templates
- [ ] Parse pub keyword on definitions
- [ ] Parse indexing (arrays)
- [ ] Parse indexing (object properties and methods)

## Examples

##### Hello World
```js
fun main() {
  print("Hello, World!"); // Hello, World!
}
```

##### Variables
```js
let name = "Jacob";
let age = 1 + 100 / 4:

print(name); // Jacob
print(age); // 26
```

##### Fibonacci
```js
// Fibonacci sequencer
fun Fibonacci(let number) {
  if (number <= 1) return number
  return Fibonacci(number - 2) + Fibonacci(number - 1)
}

print(Fibonacci(10)) // 55
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

### Building / Running

To run the project, you can run the following command:

```bash
cargo run ./path/to/file.jk
```

To run the compiler in verbose mode, you can run the following command:

```bash
cargo run ./path/to/file.jk -v
```

This will help you see the output of the lexer and parser and debug any issues you may have.

## Contributing

Contributions are very welcome. Please read the [CONTRIBUTING.md](./.github/docs/CONTRIBUTING.md) file to get acquainted with the process and guidelines. If you have any questions, feel free to ask in Jacob's project [Discord server](https://discord.gg/cWzcQz2).

## License

This project is distributed under the GPLv3 License - see the [license file](LICENSE) for details.

Copyright © 2024 Jacob Buzalski, jink-lang contributors
