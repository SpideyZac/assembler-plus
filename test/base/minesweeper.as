// r1 cursor x
// r2 cursor y
// r3 cell addr
// r4 cell data
// r5 mask
// r14 input
// r13 tmp
// r12 normal cells remaining
// r11 bombs remaining
// 4x4 cells

DEFINE LEFT 1
DEFINE DOWN 2
DEFINE RIGHT 4
DEFINE UP 8
DEFINE A 16
DEFINE B 32

DEFINE MINE_COUNT_MASK 15
DEFINE FLAG_MASK 16
DEFINE CLICKED_MASK 32
DEFINE MINE_MASK 64

DEFINE BOMB_COUNT 10

	CAL .PLACE_BOMBS
	LDI r1 7
.SETUP_X_LOOP
	LDI r2 7
.SETUP_Y_LOOP
	CAL .REDRAW_CELL
	DEC r2
	BRH GE .SETUP_Y_LOOP
	DEC r1
	BRH GE .SETUP_X_LOOP
	
	LDI r15 signed_mode
	STR r15 r0
	
	LDI r1 0
	LDI r2 0
	LDI r11 BOMB_COUNT
	LDI r15 show_number
	STR r15 r11
	LDI r12 64
	SUB r12 r11 r12

.MAIN_LOOP
	LDI r14 0 // Clear the cursor.
	CAL .DRAW_CURSOR
	LDI r15 controller_input
	LOD r15 r14
	LDI r13 UP
	AND r13 r14 r0
	BRH EQ .NOT_UP
	INC r2
.NOT_UP
	LDI r13 RIGHT
	AND r13 r14 r0
	BRH EQ .NOT_RIGHT
	INC r1
.NOT_RIGHT
	LDI r13 DOWN
	AND r13 r14 r0
	BRH EQ .NOT_DOWN
	DEC r2
.NOT_DOWN
	LDI r13 LEFT
	AND r13 r14 r0
	BRH EQ .NOT_LEFT
	DEC r1
.NOT_LEFT
	LDI r13 7
	AND r1 r13 r1
	AND r2 r13 r2

	LDI r13 A
	AND r13 r14 r0
	BRH EQ .NOT_A
	CAL .GET_ADDR
	LOD r3 r4
	LDI r5 CLICKED_MASK
	AND r4 r5 r0
	BRH NE .NOT_A
	LDI r5 FLAG_MASK
	XOR r4 r5 r4
	AND r4 r5 r0
	BRH EQ .NO_FLAG
	DEC r11
	JMP .FLAG_DONE
.NO_FLAG
	INC r11
.FLAG_DONE
	LDI r15 show_number
	STR r15 r11
	STR r3 r4
	CAL .REDRAW_CELL
.NOT_A
	
	LDI r13 B
	AND r13 r14 r0
	BRH EQ .NOT_B
	CAL .ZERO_FLOOD
.NOT_B
	
	LDI r14 1 // Draw the cursor.
	CAL .DRAW_CURSOR
	
	LDI r15 buffer_screen
	STR r15 r0
	
	CMP r12 r0
	BRH EQ .WIN

	JMP .MAIN_LOOP

.REVEAL_CELL
	CAL .GET_ADDR
	LOD r3 r4
	LDI r5 CLICKED_MASK
	AND r4 r5 r0
	BRH NE .CLICKED
	DEC r12
	XOR r4 r5 r4
.CLICKED
	LDI r5 FLAG_MASK
	AND r4 r5 r0
	BRH EQ .NOT_FLAGGED
	INC r11
	LDI r15 show_number
	STR r15 r11
.NOT_FLAGGED
	NOT r5 r5
	AND r4 r5 r4
	LDI r5 MINE_MASK
	AND r4 r5 r0
	BRH NE .GAME_OVER
	STR r3 r4
	CAL .REDRAW_CELL
	LDI r15 buffer_screen
	STR r15 r0
RET

.ZERO_FLOOD
	LDI r10 64
	STR r10 r1 0
	STR r10 r2 1
	ADI r10 2
.ZERO_FLOOD_LOOP
	LDI r9 64
	CMP r10 r9
	BRH EQ .ZERO_FLOOD_LOOP_EXIT
	ADI r10 -2
	LOD r10 r1 0
	LOD r10 r2 1
	CAL .GET_ADDR
	LOD r3 r9
	CAL .REVEAL_CELL
	CMP r9 r0 // No flag, not clicked and zero mines
	BRH NE .ZERO_FLOOD_LOOP
	DEC r1
	DEC r2
	LDI r7 3
.ZERO_FLOOD_X_LOOP
	LDI r5 8
	CMP r1 r5
	BRH GE .ZERO_FLOOD_CONTINUE_X
	LDI r8 3
.ZERO_FLOOD_Y_LOOP
	LDI r5 8
	CMP r2 r5
	BRH GE .ZERO_FLOOD_CONTINUE_Y
	STR r10 r1 0
	STR r10 r2 1
	ADI r10 2
.ZERO_FLOOD_CONTINUE_Y
	INC r2
	DEC r8
	BRH NE .ZERO_FLOOD_Y_LOOP
	ADI r2 -3
.ZERO_FLOOD_CONTINUE_X
	INC r1
	DEC r7
	BRH NE .ZERO_FLOOD_X_LOOP
	JMP .ZERO_FLOOD_LOOP
.ZERO_FLOOD_LOOP_EXIT
RET

// r14: bool draw or clear
.DRAW_CURSOR
	LSH r2 r4
	LSH r4 r4
	LDI r5 2
.X_OUTER
	LDI r15 pixel_y
	STR r15 r4
	LSH r1 r3
	LSH r3 r3
	LDI r6 5
.X_LOOP
	LDI r15 pixel_x
	STR r15 r3
	CMP r14 r0
	BRH EQ .CLEAR_X
	LDI r15 draw_pixel
	JMP .DONE_X
.CLEAR_X
	LDI r15 clear_pixel
.DONE_X
	STR r15 r0
	INC r3
	DEC r6
	BRH NE .X_LOOP
	ADI r4 4
	DEC r5
	BRH NE .X_OUTER
	
	LSH r1 r3
	LSH r3 r3
	LDI r5 2
.Y_OUTER
	LDI r15 pixel_x
	STR r15 r3
	LSH r2 r4
	LSH r4 r4
	LDI r6 5
.Y_LOOP
	LDI r15 pixel_y
	STR r15 r4
	CMP r14 r0
	BRH EQ .CLEAR_Y
	LDI r15 draw_pixel
	JMP .DONE_Y
.CLEAR_Y
	LDI r15 clear_pixel
.DONE_Y
	STR r15 r0
	INC r4
	DEC r6
	BRH NE .Y_LOOP
	ADI r3 4
	DEC r5
	BRH NE .Y_OUTER
RET

.CLEAR_CELL
	LSH r1 r3
	LSH r3 r3
	LDI r5 3
.CLEAR_X_LOOP
	INC r3
	LDI r15 pixel_x
	STR r15 r3
	LSH r2 r4
	LSH r4 r4
	LDI r6 3
.CLEAR_Y_LOOP
	INC r4
	LDI r15 pixel_y
	STR r15 r4
	LDI r15 clear_pixel
	STR r15 r0
	DEC r6
	BRH NE .CLEAR_Y_LOOP
	DEC r5
	BRH NE .CLEAR_X_LOOP
RET

.REDRAW_CELL
	CAL .CLEAR_CELL
	CAL .GET_ADDR
	LOD r3 r3
	
	LDI r4 CLICKED_MASK
	AND r3 r4 r0
	BRH NE .DRAW_CLICKED
	
	LDI r4 FLAG_MASK
	AND r3 r4 r0
	BRH NE .DRAW_FLAG
	
	JMP .DRAW_UNCLICKED
RET

// r3 = state
.DRAW_CLICKED
	LDI r4 MINE_MASK
	AND r3 r4 r0
	BRH NE .DRAW_MINE
	LDI r4 MINE_COUNT_MASK
	AND r3 r4 r3
	LSH r1 r5
	LSH r5 r5
	LSH r2 r6
	LSH r6 r6
	INC r5
	INC r6
	LDI r7 3
.DRAW_CLICKED_LOOP
	CMP r3 r0
	BRH EQ .DRAW_CLICKED_LOOP_EXIT
	CAL ._REDRAW_CELL_DRAW_PIXEL
	INC r5
	DEC r7
	BRH NE .DRAW_CLICKED_CONTINUE
	ADI r5 -3
	INC r6
.DRAW_CLICKED_CONTINUE
	DEC r3
	JMP .DRAW_CLICKED_LOOP
.DRAW_CLICKED_LOOP_EXIT
RET

.DRAW_MINE
	LSH r1 r5
	LSH r5 r5
	LSH r2 r6
	LSH r6 r6
	INC r5
	ADI r6 2
	CAL ._REDRAW_CELL_DRAW_PIXEL
	INC r5
	CAL ._REDRAW_CELL_DRAW_PIXEL
	INC r6
	CAL ._REDRAW_CELL_DRAW_PIXEL
	ADI r6 -2
	CAL ._REDRAW_CELL_DRAW_PIXEL
	INC r6
	INC r5
	CAL ._REDRAW_CELL_DRAW_PIXEL
RET

.DRAW_FLAG
	LSH r1 r5
	LSH r5 r5
	LSH r2 r6
	LSH r6 r6
	INC r5
	INC r6
	CAL ._REDRAW_CELL_DRAW_PIXEL
	INC r6
	CAL ._REDRAW_CELL_DRAW_PIXEL
	INC r6
	CAL ._REDRAW_CELL_DRAW_PIXEL
	INC r5
	CAL ._REDRAW_CELL_DRAW_PIXEL
	DEC r6
	CAL ._REDRAW_CELL_DRAW_PIXEL
	INC r5
	CAL ._REDRAW_CELL_DRAW_PIXEL
RET

.DRAW_UNCLICKED
	LSH r1 r5
	LSH r5 r5
	LDI r7 3
.UNCLICKED_X_LOOP
	INC r5
	LDI r15 pixel_x
	STR r15 r5
	LSH r2 r6
	LSH r6 r6
	LDI r8 3
.UNCLICKED_Y_LOOP
	INC r6
	LDI r15 pixel_y
	STR r15 r6
	LDI r15 draw_pixel
	STR r15 r0
	DEC r8
	BRH NE .UNCLICKED_Y_LOOP
	DEC r7
	BRH NE .UNCLICKED_X_LOOP
RET

._REDRAW_CELL_DRAW_PIXEL
	LDI r15 pixel_x
	STR r15 r5
	LDI r15 pixel_y
	STR r15 r6
	LDI r15 draw_pixel
	STR r15 r0
RET

// Get's the cells address in r3
.GET_ADDR
	LSH r1 r3
	LSH r3 r3
	LSH r3 r3
	ADD r3 r2 r3
RET

.PLACE_BOMBS
	LDI r9 BOMB_COUNT
.BOMB_LOOP
	LDI r15 rng
	LOD r15 r4
	LOD r15 r5
	LDI r6 7
	AND r4 r6 r4
	AND r5 r6 r5
	MOV r1 r11
	MOV r2 r12
	MOV r4 r1
	MOV r5 r2
	CAL .GET_ADDR
	MOV r11 r1
	MOV r12 r2
	LOD r3 r11
	LDI r12 MINE_MASK
	AND r11 r12 r0
	BRH NE .BOMB_LOOP
	XOR r11 r12 r11
	STR r3 r11
	DEC r4
	DEC r5
	LDI r6 3
.BOMB_X_LOOP
	LDI r7 3
.BOMB_Y_LOOP
	LDI r8 8
	CMP r4 r8
	BRH GE .CONTINUE
	CMP r5 r8
	BRH GE .CONTINUE
	MOV r1 r11
	MOV r2 r12
	MOV r4 r1
	MOV r5 r2
	CAL .GET_ADDR
	MOV r11 r1
	MOV r12 r2
	LOD r3 r10
	INC r10
	STR r3 r10
.CONTINUE
	INC r5
	DEC r7
	BRH NE .BOMB_Y_LOOP
	INC r4
	ADI r5 -3
	DEC r6
	BRH NE .BOMB_X_LOOP
	DEC r9
	BRH NE .BOMB_LOOP
RET

.GAME_OVER
	LDI r1 7
.GAME_OVER_X_LOOP
	LDI r2 7
.GAME_OVER_Y_LOOP
	CAL .GET_ADDR
	LOD r3 r4
	LDI r5 MINE_MASK
	AND r4 r5 r0
	BRH EQ .NO_MINE
	LDI r5 FLAG_MASK
	AND r4 r5 r0
	BRH NE .NO_MINE
	CAL .CLEAR_CELL
	CAL .DRAW_MINE
.NO_MINE
	DEC r2
	BRH GE .GAME_OVER_Y_LOOP
	DEC r1
	BRH GE .GAME_OVER_X_LOOP
	LDI r15 buffer_screen
	STR r15 r0
	LDI r15 clear_chars_buffer
	STR r15 r0
	LDI r15 write_char
	LDI r1 'G'
	STR r15 r1
	LDI r1 'A'
	STR r15 r1
	LDI r1 'M'
	STR r15 r1
	LDI r1 'E'
	STR r15 r1
	LDI r1 ' '
	STR r15 r1
	LDI r1 'O'
	STR r15 r1
	LDI r1 'V'
	STR r15 r1
	LDI r1 'E'
	STR r15 r1
	LDI r1 'R'
	STR r15 r1
	LDI r15 buffer_chars
	STR r15 r0
HLT

.WIN
	LDI r15 clear_chars_buffer
	STR r15 r0
	LDI r15 write_char
	LDI r1 'Y'
	STR r15 r1
	LDI r1 'O'
	STR r15 r1
	LDI r1 'U'
	STR r15 r1
	LDI r1 ' '
	STR r15 r1
	LDI r1 'W'
	STR r15 r1
	LDI r1 'O'
	STR r15 r1
	LDI r1 'N'
	STR r15 r1
	LDI r15 buffer_chars
	STR r15 r0
HLT