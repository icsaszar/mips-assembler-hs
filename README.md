# mips-stack

Configurable mips assembler written in haskell.

## Installation

In order to build the project `stack` is required.

```
git clone https://github.com/icsaszar/mips-assembler-hs.git
cd mips-assembler
stack build
```

## Usage

```
mips-assembler-exe -s res/test.mips
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
