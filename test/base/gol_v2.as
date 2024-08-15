// r1 - x
// r2 - y
// r3 - neighbor sum
// r4 - state
// r5 - debug x
// r6 - debug y
// r7 - gen counter
// r12 - buffer cell state


// define boundary 224 // 11100000 32x32
define boundary 248 // 11111000 8x8
// define boundary 252 // 11111100 4x4

LDI r15 clear_chars_buffer // write "life"
STR r15 r0
LDI r15 write_char
LDI r14 "L"
STR r15 r14
LDI r14 "I"
STR r15 r14
LDI r14 "F"
STR r15 r14
LDI r14 "E"
STR r15 r14
LDI r15 buffer_chars
STR r15 r0
LDI r15 clear_number // 0 to number display
STR r15 r0
LDI r15 unsigned_mode
STR r15 r0
LDI r7 0
LDI r15 show_number
STR r15 r7

// initial pattern
LDI r15 clear_screen_buffer // clear screen
STR r15 r0

CAL .initial_pattern

LDI r15 buffer_screen
STR r15 r0

// temp
LDI r1 0
LDI r2 0

.gol_loop
CAL .sum_neighbors
LDI r15 pixel_x
STR r15 r1
LDI r15 pixel_y
STR r15 r2
LDI r15 load_pixel
LOD r15 r4
//LDI r15 clear_pixel
//STR r15 r0
NOR r4 r3 r14
LDI r13 252 // 11111100
CMP r14 r13
BRH ne .not_birth
//CAL .xy_to_ram
//LDI r15 draw_pixel
//STR r15 r0
CAL .draw_to_row_buffer
.not_birth

ADI r1 1
LDI r13 boundary
AND r1 r13 r14
CMP r14 r0
BRH eq .gol_loop
CMP r2 r0
BRH eq .is_first_row
CAL .apply_row_buffer
.is_first_row
CAL .shift_buffers
//LDI r15 buffer_screen
//STR r15 r0
LDI r1 0
ADI r2 1
LDI r13 boundary
AND r2 r13 r14
CMP r14 r0
BRH eq .gol_loop

CAL .apply_row_buffer
LDI r2 0


ADI r7 1
LDI r15 show_number
STR r15 r7

LDI r15 buffer_screen
STR r15 r0
//HLT
JMP .gol_loop




HLT

.sum_neighbors
MOV r1 r5
MOV r2 r6
LDI r3 0
ADI r1 1
CAL .sum_neighbor
ADI r2 1
CAL .sum_neighbor
ADI r1 -1
CAL .sum_neighbor
ADI r1 -1
CAL .sum_neighbor
ADI r2 -1
CAL .sum_neighbor
ADI r2 -1
CAL .sum_neighbor
ADI r1 1
CAL .sum_neighbor
ADI r1 1
CAL .sum_neighbor
ADI r1 -1
ADI r2 1
RET


.sum_neighbor
LDI r13 boundary
AND r1 r13 r14
CMP r14 r0
BRH ne .sum_neighbor_ret
AND r2 r13 r14
CMP r14 r0
BRH ne .sum_neighbor_ret
LDI r15 pixel_x
STR r15 r1
LDI r15 pixel_y
STR r15 r2
LDI r15 load_pixel
LOD r15 r14
CMP r14 r0
BRH eq .sum_neighbor_ret
ADI r3 1
.sum_neighbor_ret
RET



.draw_to_row_buffer
LDI r13 1
STR r1 r13
RET

.shift_buffers
LDI r13 0
LDI r14 32
.buffer_shift_loop
ADD r13 r14 r15
LOD r13 r12
STR r15 r12
STR r13 r0
ADI r13 1
CMP r13 r14
BRH ne .buffer_shift_loop
RET

.apply_row_buffer
LDI r13 0
LDI r14 32
.buffer_shift_loop_2
ADD r13 r14 r15
LOD r15 r12
LDI r15 pixel_x
STR r15 r13
LDI r15 pixel_y
ADI r2 -1
STR r15 r2
ADI r2 1
LDI r15 clear_pixel
STR r15 r0
CMP r12 r0
BRH eq .no_birth_from_buffer
LDI r15 draw_pixel
STR r15 r0
.no_birth_from_buffer
ADI r13 1
CMP r13 r14
BRH ne .buffer_shift_loop_2
RET


.initial_pattern
LDI r15 pixel_x
LDI r14 0
STR r15 r14
LDI r15 pixel_y
LDI r14 0
STR r15 r14
LDI r15 draw_pixel
STR r15 r0

LDI r15 pixel_x
LDI r14 1
STR r15 r14
LDI r15 pixel_y
LDI r14 1
STR r15 r14
LDI r15 draw_pixel
STR r15 r0

LDI r15 pixel_x
LDI r14 1
STR r15 r14
LDI r15 pixel_y
LDI r14 2
STR r15 r14
LDI r15 draw_pixel
STR r15 r0

LDI r15 pixel_x
LDI r14 2
STR r15 r14
LDI r15 pixel_y
LDI r14 0
STR r15 r14
LDI r15 draw_pixel
STR r15 r0

LDI r15 pixel_x
LDI r14 2
STR r15 r14
LDI r15 pixel_y
LDI r14 1
STR r15 r14
LDI r15 draw_pixel
STR r15 r0

RET