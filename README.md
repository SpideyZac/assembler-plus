# Assembler+
An offset of the [Batpu2 assembler](https://github.com/mattbatwings/newcpu/blob/main/assembler.py) written in Rust with a multitude of useful additions/features.

## Features
### Macros 🟢
Define reusable code blocks that can be inserted into your code with arguments

```asm
%macro WRITE_CHAR char
ldi r1 $char
ldi r2 247
str r2 r1 0
%endmacro

%WRITE_CHAR 5
```

## Conditional ASM 🟡
Conditional ASM allows you to include or exclude certain code blocks if a user defined condition is met

```asm
define DEBUG 1

%if DEBUG == 1
hlt
%endif

%ifdef DEBUG
hlt
%endif
```

## Include Files 🟢
The include files feature allows you to import another file into your code.

```asm
%include "abc.asm"
cal .abc123
```

## Compile time arithmatic 🟡 - Maybe not possible now
Compile time arithmatic allows you to perform arithmatic on compile time known values (constants, registers with known values, memory slots with known values, etc.)

```asm
define a 1
ldi r1 a + a
ldi r2 $reg r1 + a
str r1 r2 0
ldi r3 $mem $reg r1
```

## Other Features
* Add standard library 🟡
* Register templating 🟢
* Add warnings 🟡
* Optimizer 🔴

Please create an issue for any suggested features.

## Usage
```bash
assembler-plus [input file path] [output file path]
```

## Compiling
```bash
cargo build --release
```

## Download
Soon!