// r1 - ant x
// r2 - ant y
// r4 - direction
// r5 - pixel at position
// r8-r11 directions
// r12 = const 31 screen wrap

define LEFT 1
define DOWN 2
define RIGHT 4
define UP 8

LDI r15 clear_screen_buffer // clear screen
STR r15 r0
LDI r15 buffer_screen
STR r15 r0

LDI r1 9
LDI r2 14
LDI r3 0
LDI r4 UP
LDI r12 31

LDI r8 LEFT
LDI r9 DOWN
LDI r10 RIGHT
LDI r11 UP

.ant_loop

LDI r15 pixel_x
STR r15 r1
LDI r15 pixel_y
STR r15 r2
LDI r15 load_pixel
LOD r15 r5

CMP r5 r0
BRH eq .cell_is_zero
.cell_is_one
LDI r15 clear_pixel
STR r15 r0
CAL .rotate_ccw
JMP .end_cell_condition
.cell_is_zero
LDI r15 draw_pixel
STR r15 r0
CAL .rotate_cw
.end_cell_condition

LDI r15 buffer_screen
STR r15 r0

CAL .move_forward_with_wrap

JMP .ant_loop


.rotate_cw
RSH r4 r4
CMP r4 r0
BRH ne .no_cw_cycle
LDI r4 UP
.no_cw_cycle
RET


.rotate_ccw
LSH r4 r4
LDI r13 15
AND r4 r13 r4
CMP r4 r0
BRH ne .no_ccw_cycle
LDI r4 LEFT
.no_ccw_cycle
RET


.move_forward_with_wrap
CMP r4 r8
BRH ne .not_move_left
ADI r1 -1
.not_move_left
CMP r4 r9
BRH ne .not_move_down
ADI r2 -1
.not_move_down
CMP r4 r10
BRH ne .not_move_right
ADI r1 1
.not_move_right
CMP r4 r11
BRH ne .not_move_up
ADI r2 1
.not_move_up
AND r1 r12 r1
AND r2 r12 r2
RET
