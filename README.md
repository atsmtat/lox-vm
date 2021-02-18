# lox-vm

lox-vm is a byte code virtual machine for the Lox language, defined and implemented [here](https://craftinginterpreters.com/) by Bob Nystrom. This is my attempt at implementing the interpreter in Rust.


## Install ##
Once you have installed [Rust](https://www.rust-lang.org/tools/install), lox-vm can be installed using `cargo`
```
$ cargo install --git https://github.com/atsmtat/lox-vm.git --branch main
```

## Build ##
lox-vm is written in Rust, so you need [Rust](https://www.rust-lang.org/tools/install) installed in order to compile it.
To build lox-vm

```
$ git clone https://github.com/atsmtat/lox-vm.git
$ cd lox-vm
$ cargo build
```

To enable disassembler, use
```
$ cargo build --features "disassemble"
```

To run tests
```
$ cargo test
```
