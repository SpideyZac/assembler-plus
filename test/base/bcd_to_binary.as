ldi r1 1 # 1st digit
str r0 r1
ldi r1 2 # 2nd digit
str r0 r1 1
ldi r1 3 # 3rd digit
str r0 r1 2
ldi r1 10 # non-digit means end of number
str r0 r1 3

ldi r1 0 # r1: pointer to start of number

ldi r2 10 # r2: check digit
ldi r3 0 # r3: result
.bcd_to_bin_loop
    lod r1 r4
    cmp r4 r2
    brh ge .end_bcd_to_bin_loop
    lsh r3 r3
    add r3 r4 r5
    lsh r3 r3
    lsh r3 r3
    add r3 r5 r3
    inc r1
    jmp .bcd_to_bin_loop
.end_bcd_to_bin_loop

ldi r15 250 # show number
str r15 r3
hlt