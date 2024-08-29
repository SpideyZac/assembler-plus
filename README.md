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

WRITE_CHAR 5
```

You can also define macro's that can take infinite argument's

```asm
%macro PUSH regs +
    %for reg in $regs
        STR r14 $reg
        INC r14
    %endfor
%endmacro
```

### Conditional ASM 🟢
Conditional ASM allows you to include or exclude certain code blocks if a user defined condition is met

```asm
define DEBUG 1
define LOG_LEVEL 0

%if LOG_LEVEL == 1
hlt
%elifdef DEBUG
nop
%else
add r1 r2 r3
%endif

%ifdef DEBUG
hlt
%elif LOG_LEVEL == 1
nop
%else
add r1 r2 r3
%endif
```

### For loops 🟢
For loops allows you to reuse code in a loop

```asm
ldi r1 clear_chars_buffer
str r1 r0
ldi r1 write_char
%for c in "Hello!"
    ldi r2 $c
    str r1 r2
%endfor
ldi r1 buffer_chars
str r1 r0
```

You can also use multiple values in a for loop

```asm
%for val in 1 2 3 0xff
    ldi r1 $val
%endfor
```

### Include Files 🟢
The include files feature allows you to import another file into your code.

```asm
%include "abc.asm"
cal .abc123
```

### Compile time arithmatic 🟡 - Maybe not possible now
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
[Go to the most recent release](https://github.com/SpideyZac/assembler-plus/releases/latest)

### WINDOWS DEFENDER WARNINGS!!!
Windows Defender detects asm+ as a Trojan. I will also inform you that it is the only AV to do so and is most likely do to we are not buying a program license. This program is completely safe.

## Special Thanks to NoName_Official!
I could not have finished this project, or gotten nearly as many features complete without him!