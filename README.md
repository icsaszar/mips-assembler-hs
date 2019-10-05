# mips-assembler-hs

Configurable mips assembler written in haskell.

The main goal of this project is to make it easier for anyone who wants to implement a MIPS processor
to write assembly code when they test their implementation, since translating assembly by hand to machine code is slow,
hard and error-prone.

## Installation

In order to build the project the `stack` build tool (usually named `haskell-stack`) is required.

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

### Unit tests

There are some basic unit tests written using Tasty and HUnit.

```
stack test
```

### Blackbox tetst

The blackbox tests are written in python with the pytest library.

```
cd ./test
pytest
```
