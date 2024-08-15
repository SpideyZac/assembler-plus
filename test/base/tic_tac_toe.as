// Tic Tac Toe by zPippo

// x,y to ram address formula:
// y * 3 + x

ldi r15 rng
lod r15 r1
ldi r3 1
and r1 r3 r3
brh zero .continue_setup
ldi r3 255
.continue_setup
ldi r4 0 // selector x
ldi r5 0 // selector y
ldi r8 9 // empty cells

ldi r15 clear_chars_buffer
str r15 r0
ldi r15 write_char
ldi r1 "T"
str r15 r1
ldi r1 "i"
str r15 r1
ldi r1 "c"
str r15 r1
ldi r1 "T"
str r15 r1
ldi r1 "a"
str r15 r1
ldi r1 "c"
str r15 r1
ldi r1 "T"
str r15 r1
ldi r1 "o"
str r15 r1
ldi r1 "e"
str r15 r1
ldi r1 " "
str r15 r1
ldi r15 buffer_chars
str r15 r0

ldi r10 clear_pixel
ldi r11 controller_input
ldi r12 pixel_x
ldi r13 pixel_y
ldi r14 draw_pixel
ldi r15 buffer_screen

ldi r6 9
ldi r1 10
.next_empty_ram
	dec r1
	dec r6
	str r6 r1
	brh notzero .next_empty_ram

ldi r6 28
ldi r2 4
ldi r1 12
.column_1
	str r12 r1
	str r13 r2
	str r14 r0
	inc r1
	str r12 r1
	str r14 r0
	dec r1
	inc r2
	dec r6
	brh notzero .column_1

ldi r6 28
ldi r2 4
ldi r1 22
.column_2
	str r12 r1
	str r13 r2
	str r14 r0
	inc r1
	str r12 r1
	str r14 r0
	dec r1
	inc r2
	dec r6
	brh notzero .column_2


ldi r6 28
ldi r2 4
ldi r1 12
.row_1
	str r12 r2
	str r13 r1
	str r14 r0
	inc r1
	str r13 r1
	str r14 r0
	dec r1
	inc r2
	dec r6
	brh notzero .row_1

ldi r6 28
ldi r2 4
ldi r1 22
.row_2
	str r12 r2
	str r13 r1
	str r14 r0
	inc r1
	str r13 r1
	str r14 r0
	dec r1
	inc r2
	dec r6
	brh notzero .row_2


.game_loop
cal .render_turn_block
cal .move_x_selector
cal .move_y_selector
cal .render_selector
cal .place_block
str r15 r0
cal .check_win
mov r8 r8
brh notzero .skip_draw
jmp .draw
.skip_draw

jmp .game_loop


.change_turn
not r3 r3
ret


.draw
ldi r15 clear_chars_buffer
str r15 r0
ldi r15 write_char
ldi r1 "D"
str r15 r1
ldi r1 "r"
str r15 r1
ldi r1 "a"
str r15 r1
ldi r1 "w"
str r15 r1
ldi r1 "!"
str r15 r1
ldi r1 " "
str r15 r1
ldi r1 " "
str r15 r1
ldi r1 " "
str r15 r1
ldi r1 " "
str r15 r1
ldi r1 " "
str r15 r1
ldi r15 buffer_chars
str r15 r0
hlt


.check_win
ldi r6 3
ldi r2 0
.check_next_column
	lod r2 r9
	adi r2 3
	lod r2 r1
	adi r2 -3
	cmp r9 r1
	brh notzero .skip_top_pixel
	adi r2 6
	lod r2 r1
	adi r2 -6
	cmp r9 r1
	brh zero .win
	.skip_top_pixel
	inc r2
	dec r6
	brh notzero .check_next_column
	
ldi r6 3
ldi r2 0
.check_next_row
	lod r2 r9
	adi r2 1
	lod r2 r1
	adi r2 -1
	cmp r9 r1
	brh notzero .skip_right_pixel
	adi r2 2
	lod r2 r1
	adi r2 -2
	cmp r9 r1
	brh zero .win
	.skip_right_pixel
	adi r2 3
	dec r6
	brh notzero .check_next_row
	
ldi r2 0
lod r2 r9
ldi r2 4
lod r2 r1
cmp r9 r1
brh notzero .skip_diagonal_1
ldi r2 8
lod r2 r1
cmp r9 r1
brh zero .win
.skip_diagonal_1
ldi r2 2
lod r2 r9
ldi r2 4
lod r2 r1
cmp r9 r1
brh notzero .skip_diagonal_2
ldi r2 6
lod r2 r1
cmp r9 r1
brh zero .win
.skip_diagonal_2

ret


.win
ldi r15 clear_chars_buffer
str r15 r0
ldi r15 write_char
mov r9 r9
brh zero .x_win
ldi r1 "O"
str r15 r1
jmp .skip_x_win
.x_win
ldi r1 "X"
str r15 r1
.skip_x_win
ldi r1 " "
str r15 r1
ldi r1 "w"
str r15 r1
ldi r1 "i"
str r15 r1
ldi r1 "n"
str r15 r1
ldi r1 "s"
str r15 r1
ldi r1 "!"
str r15 r1
ldi r15 buffer_chars
str r15 r0
hlt


.render_turn_block
ldi r1 1
ldi r6 3
.clear_next_pixel_x_block_small
	ldi r2 1
	ldi r7 3
	.clear_next_pixel_y_block_small
		str r12 r1
		str r13 r2
		str r10 r0
		inc r2
		dec r7
		brh notzero .clear_next_pixel_y_block_small
	inc r1
	dec r6
	brh notzero .clear_next_pixel_x_block_small

ldi r1 1
ldi r2 1	
mov r3 r3
brh zero .x_block_small
cal .render_o_block_small
jmp .skip_x_block_small
.x_block_small
cal .render_x_block_small
.skip_x_block_small
ret


.move_x_selector
lod r11 r9 // get input
ldi r1 1
and r9 r1 r1
brh zero .skip_move_left
dec r4
.skip_move_left
ldi r1 4
and r9 r1 r1
brh zero .skip_move_right
inc r4
.skip_move_right
ldi r1 255
cmp r1 r4
brh notzero .skip_fix_left
ldi r4 0
.skip_fix_left
ldi r1 3
cmp r1 r4
brh notzero .skip_fix_right
ldi r4 2
.skip_fix_right
ret


.move_y_selector
lod r11 r9 // get input
ldi r1 2
and r9 r1 r1
brh zero .skip_move_down
dec r5
.skip_move_down
ldi r1 8
and r9 r1 r1
brh zero .skip_move_up
inc r5
.skip_move_up
ldi r1 255
cmp r1 r5
brh notzero .skip_fix_down
ldi r5 0
.skip_fix_down
ldi r1 3
cmp r1 r5
brh notzero .skip_fix_up
ldi r5 2
.skip_fix_up
ret


.render_selector
ldi r1 1
str r13 r1
ldi r1 5
ldi r6 26
.clear_next_pixel_x_selector
	str r12 r1
	str r10 r0
	inc r1
	dec r6
	brh notzero .clear_next_pixel_x_selector
ldi r1 1
str r12 r1
ldi r1 5
ldi r6 26
.clear_next_pixel_y_selector
	str r13 r1
	str r10 r0
	inc r1
	dec r6
	brh notzero .clear_next_pixel_y_selector

mov r4 r1
cal .multiply_10
cal .draw_x_selector
mov r5 r1
cal .multiply_10
cal .draw_y_selector
ret


.multiply_10
mov r1 r2
lsh r1 r1
lsh r1 r1
lsh r1 r1
lsh r2 r2
add r1 r2 r1
adi r1 5
ldi r2 1
ret


.draw_x_selector
ldi r6 6
.draw_next_pixel_x_selector
	str r12 r1
	str r13 r2
	str r14 r0
	inc r1
	dec r6
	brh notzero .draw_next_pixel_x_selector
ret


.draw_y_selector
ldi r6 6
.draw_next_pixel_y_selector
	str r12 r2
	str r13 r1
	str r14 r0
	inc r1
	dec r6
	brh notzero .draw_next_pixel_y_selector
ret


.place_block
lod r11 r9
ldi r1 32
cmp r9 r1
brh notzero .skip_place_block
mov r4 r1
mov r5 r2
lsh r2 r2
add r5 r2 r2
add r1 r2 r1
lod r1 r2
cmp r0 r2
brh notzero .continue_place_block_check
ret
.continue_place_block_check
ldi r6 255
cmp r6 r2
brh notzero .continue_place_block
ret
.continue_place_block
dec r8
str r1 r3
mov r4 r1
cal .multiply_10
mov r1 r6
mov r5 r1
cal .multiply_10
mov r1 r2
mov r6 r1
mov r3 r3
brh zero .x_block
cal .render_o_block
jmp .skip_x_block
.x_block
cal .render_x_block
.skip_x_block
cal .change_turn
.skip_place_block
ret


.render_o_block
ldi r6 6
.next_pixel_bottom_row
	str r12 r1
	str r13 r2
	str r14 r0
	inc r2
	str r13 r2
	str r14 r0
	dec r2
	inc r1
	dec r6
	brh notzero .next_pixel_bottom_row

adi r1 -6
adi r2 2
str r12 r1
str r13 r2
str r14 r0
inc r2
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
dec r2
str r13 r2
str r14 r0
adi r1 3
str r12 r1
str r14 r0
inc r2
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
dec r2
str r13 r2
str r14 r0

adi r1 -5
adi r2 2
ldi r6 6
.next_pixel_top_row
	str r12 r1
	str r13 r2
	str r14 r0
	inc r2
	str r13 r2
	str r14 r0
	dec r2
	inc r1
	dec r6
	brh notzero .next_pixel_top_row
ret


.render_x_block
str r12 r1
str r13 r2
str r14 r0
inc r2
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
dec r2
str r13 r2
str r14 r0
adi r1 3
str r12 r1
str r14 r0
inc r2
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
dec r2
str r13 r2
str r14 r0

adi r1 -3
adi r2 2
str r12 r1
str r13 r2
str r14 r0
inc r2
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
dec r2
str r13 r2
str r14 r0

adi r1 -3
adi r2 2
str r12 r1
str r13 r2
str r14 r0
inc r2
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
dec r2
str r13 r2
str r14 r0
adi r1 3
str r12 r1
str r14 r0
inc r2
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
dec r2
str r13 r2
str r14 r0
ret


.render_o_block_small
str r12 r1
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
inc r1
str r12 r1
str r14 r0
inc r2
str r13 r2
str r14 r0
adi r1 -2
str r12 r1
str r14 r0
inc r2
str r13 r2
str r14 r0
inc r1
str r12 r1
str r14 r0
inc r1
str r12 r1
str r14 r0
ret


.render_x_block_small
str r12 r1
str r13 r2
str r14 r0
adi r1 2
str r12 r1
str r14 r0
dec r1
inc r2
str r12 r1
str r13 r2
str r14 r0
dec r1
inc r2
str r12 r1
str r13 r2
str r14 r0
adi r1 2
str r12 r1
str r14 r0
ret

