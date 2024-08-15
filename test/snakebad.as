// SNAKE by mattbatwings

// r1 - head x
// r2 - head y
// r3 - tail x
// r4 - tail y
// r5 - apple x
// r6 - apple y
// r7 - head pointer
// r8 - tail pointer
// r9 - score

define LEFT 1
define DOWN 2
define RIGHT 4
define UP 8

CAL .setup

.loop 
LDI r15 pixel_x
LSH r5 r14
STR r15 r14
LDI r15 pixel_y
LSH r6 r14
STR r15 r14
LDI r15 clear_pixel
STR r15 r0 
CMP r1 r5
BRH ne .not_equal
CMP r2 r6
BRH ne .not_equal

.equal 
CAL .inc_score
CAL .gen_apple
JMP .continue

.not_equal 
CAL .update_tail

.continue 
CAL .check_lose
CAL .update_head
CAL .check_win

LDI r15 pixel_x
LSH r5 r14
STR r15 r14
LDI r15 pixel_y
LSH r6 r14
STR r15 r14
LDI r15 draw_pixel
STR r15 r0
LDI r15 buffer_screen
STR r15 r0
JMP .loop

.check_win 
CMP r7 r8
BRH ne .not_win
LDI r15 clear_chars_buffer
STR r15 r0
LDI r15 write_char
LDI r14 "Y"
STR r15 r14
LDI r14 "O"
STR r15 r14
LDI r14 "U"
STR r15 r14
LDI r14 " "
STR r15 r14
LDI r14 "W"
STR r15 r14
LDI r14 "I"
STR r15 r14
LDI r14 "N"
STR r15 r14
LDI r15 buffer_chars
STR r15 r0
HLT
.not_win 
RET

.check_lose 
LDI r14 -1 // hit boundary
LDI r15 16
CMP r1 r14
BRH eq .lose
CMP r1 r15
BRH eq .lose
CMP r2 r14
BRH eq .lose  
CMP r2 r15
BRH eq .lose
LDI r15 pixel_x // hit self
LSH r1 r14
STR r15 r14
LDI r15 pixel_y
LSH r2 r14
STR r15 r14
LDI r15 load_pixel
LOD r15 r14
CMP r14 r0
BRH ne .lose
JMP .not_lose
.lose 
LDI r15 clear_chars_buffer
STR r15 r0
LDI r15 write_char
LDI r14 "Y"
STR r15 r14
LDI r14 "O"
STR r15 r14
LDI r14 "U"
STR r15 r14
LDI r14 " "
STR r15 r14
LDI r14 "L"
STR r15 r14
LDI r14 "O"
STR r15 r14
LDI r14 "S"
STR r15 r14
LDI r14 "E"
STR r15 r14
LDI r15 buffer_chars
STR r15 r0 
HLT
.not_lose 
RET

.setup 
LDI r15 clear_screen_buffer // clear screen
STR r15 r0
LDI r15 buffer_screen
STR r15 r0
LDI r15 clear_chars_buffer // write "snake"
STR r15 r0
LDI r15 write_char
LDI r14 "S"
STR r15 r14
LDI r14 "N"
STR r15 r14
LDI r14 "A"
STR r15 r14
LDI r14 "K"
STR r15 r14
LDI r14 "E"
STR r15 r14
LDI r15 buffer_chars
STR r15 r0
LDI r15 clear_number // write 0 to number display
STR r15 r0
LDI r15 unsigned_mode
STR r15 r0
LDI r15 show_number
STR r15 r0
LDI r1 7 // head at (7, 7)
LDI r2 7 
LDI r3 7 // tail at (7, 6)
LDI r4 6 
CAL .gen_apple
LDI r7 1
LDI r8 0
LDI r15 UP
LDI r10 UP
STR r8 r15 
RET

.update_head 
LDI r15 controller_input // get input
LOD r15 r11
LDI r14 15 // bitmask for dpad
AND r14 r11 r11
CMP r11 r10
BRH eq .no_move
CMP r11 r0
BRH eq .no_move
MOV r11 r10
.no_move 
STR r7 r10 // place head 
INC r7 
LDI r15 pixel_x
LSH r1 r14 
STR r15 r14
LDI r15 pixel_y
LSH r2 r14 
STR r15 r14
LDI r15 draw_pixel
STR r15 r0
LDI r14 LEFT // update head xy
CMP r10 r14
BRH ne .not_left_head
DEC r1
.not_left_head 
LDI r14 DOWN
CMP r10 r14
BRH ne .not_down_head
DEC r2
.not_down_head 
LDI r14 RIGHT
CMP r10 r14
BRH ne .not_right_head
INC r1
.not_right_head 
LDI r14 UP
CMP r10 r14
BRH ne .not_up_head
INC r2
.not_up_head 
RET

.update_tail 
LDI r15 pixel_x // clear pixel on screen
LSH r3 r14
STR r15 r14
LDI r15 pixel_y
LSH r4 r14
STR r15 r14
LDI r15 clear_pixel 
STR r15 r0
LOD r8 r15 // update tail pointer
STR r8 r0 // clear mem at tail
INC r8
LDI r14 LEFT // update tail xy
CMP r15 r14
BRH ne .not_left_tail
DEC r3
.not_left_tail 
LDI r14 DOWN
CMP r15 r14
BRH ne .not_down_tail
DEC r4
.not_down_tail 
LDI r14 RIGHT
CMP r15 r14
BRH ne .not_right_tail
INC r3
.not_right_tail 
LDI r14 UP
CMP r15 r14
BRH ne .not_up_tail
INC r4
.not_up_tail 
RET

.inc_score 
INC r9
LDI r15 show_number
STR r15 r9
RET

.gen_apple 
LDI r15 rng
LDI r14 15
LOD r15 r5
AND r14 r5 r5
LDI r15 rng
LOD r15 r6
AND r14 r6 r6
RET