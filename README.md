# mips-assembler-hs

Configurable mips assembler written in haskell.

The main goal of this project is to make it easier for anyone who wants to implement a mips processor
to write assembly code when they test their implementation, since translating assembly by hand to machine code is slow,
hard and error-prone.

## Features

Currently the assembler only supports a subset of instructions (which are usually implemented by students), labels and comments.

Section declarations or variables are not supported.

Error handling is quite limited.

The code should work on both Unix and Windows systems.

## Installation

In order to build the project the [stack](https://docs.haskellstack.org/en/stable/README/)
build tool (usually named `haskell-stack`) is required.

```
git clone https://github.com/icsaszar/mips-assembler-hs.git
cd mips-assembler-hs
stack build
```

## Usage

### Running using `stack run`

```
stack run -- --sep res/test.mips
```

### Running the executable directly

```
mips-assembler-exe --sep res/test.mips
```

## Changing the opcodes and alu functions

You can edit the opcodes and alu functions in the `config/codes.json` file to match with your mips design.

## Testing

Running the tests after building the project is encouraged.

### Unit tests

There are some basic unit tests written using [Tasty](http://hackage.haskell.org/package/tasty) and 
[HUnit](http://hackage.haskell.org/package/tasty-hunit).

```
stack test
```

### Blackbox tetst

The blackbox tests are written in python with the [pytest](https://docs.pytest.org/en/latest/) library.

```
cd ./test
pytest
```
