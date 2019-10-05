# mips-assembler-hs

Configurable mips assembler written in haskell.

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
