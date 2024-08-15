// Flappy Bird by zPippo

ldi r3 0 // score
ldi r4 120 // bird y
ldi r5 0 // y velocity
ldi r6 -3 // gravity

ldi r15 clear_chars_buffer
str r15 r0
ldi r15 write_char
ldi r1 "F"
str r15 r1
ldi r1 "l"
str r15 r1
ldi r1 "a"
str r15 r1
ldi r1 "p"
str r15 r1
ldi r1 "p"
str r15 r1
ldi r1 "y"
str r15 r1
ldi r1 "B"
str r15 r1
ldi r1 "i"
str r15 r1
ldi r1 "r"
str r15 r1
ldi r1 "d"
str r15 r1
ldi r15 buffer_chars
str r15 r0


ldi r10 controller_input
ldi r11 clear_screen_buffer
ldi r12 pixel_x
ldi r13 pixel_y
ldi r14 draw_pixel

ldi r1 10
ldi r2 125 // 0b01111101
           // 0110
		   // 1111
str r1 r2

ldi r1 42
ldi r2 8
ldi r8 7
.generate_next_pipe
	dec r2
	str r2 r1
	adi r1 -10
	ldi r7 rng
	lod r7 r7
	and r7 r8 r7
	adi r7 8
	dec r2
	str r2 r7
	brh notzero .generate_next_pipe


.game_loop
cal .jump
str r11 r0
cal .update_score
cal .movement
cal .draw_pipes
cal .draw_bird
ldi r15 buffer_screen
str r15 r0
cal .collisions
brh zero .skip_lose
jmp .lose
.skip_lose
ldi r1 255
cmp r1 r3
brh notzero .skip_win
jmp .win
.skip_win
cal .move_pipes

jmp .game_loop


.jump
lod r10 r1
ldi r2 8
and r2 r1 r1
brh zero .skip_jump
ldi r5 8
.skip_jump
ret


.draw_bird
mov r4 r9
rsh r9 r9
rsh r9 r9
rsh r9 r9
ldi r8 6
ldi r1 10
lod r1 r1
ldi r2 1
ldi r7 4
.next_pixel_bird
	and r1 r2 r0
	brh zero .skip_pixel_bird_1
	str r12 r8
	str r13 r9
	str r14 r0
	.skip_pixel_bird_1
	
	inc r9
	lsh r2 r2
	and r1 r2 r0
	brh zero .skip_pixel_bird_2
	str r12 r8
	str r13 r9
	str r14 r0
	.skip_pixel_bird_2
	
	lsh r2 r2
	dec r9
	inc r8
	
	dec r7
	brh notzero .next_pixel_bird

ret


.draw_pipes
ldi r7 8
ldi r9 32
.next_pipe
	dec r7
	lod r7 r2
	dec r7
	lod r7 r8
	cmp r2 r9
	brh ge .skip_draw_bottom_left_pipe_pixel
	mov r8 r1
	str r12 r2
	.next_pixel_pipe_bottom_left
		dec r1
		
		str r13 r1
		str r14 r0
		
		brh notzero .next_pixel_pipe_bottom_left
	
	.skip_draw_bottom_left_pipe_pixel
	
	inc r2
	cmp r2 r9
	brh ge .skip_draw_bottom_right_pipe_pixel
	mov r8 r1
	str r12 r2
	.next_pixel_pipe_bottom_right
		dec r1
		
		str r13 r1
		str r14 r0
		
		brh notzero .next_pixel_pipe_bottom_right
	
	.skip_draw_bottom_right_pipe_pixel
	
	adi r2 -2
	cmp r2 r9
	brh ge .skip_pipe_exit_left_pixels
	str r12 r2
	mov r8 r1
	dec r1
	str r13 r1
	str r14 r0
	adi r1 11
	str r13 r1
	str r14 r0
	adi r1 -11
	.skip_pipe_exit_left_pixels
	adi r2 3
	cmp r2 r9
	brh ge .skip_pipe_exit_right_pixels
	str r12 r2
	mov r8 r1
	dec r1
	str r13 r1
	str r14 r0
	adi r1 11
	str r13 r1
	str r14 r0
	adi r1 -11
	.skip_pipe_exit_right_pixels
	adi r1 11
	adi r2 -2
	
	cmp r2 r9
	brh ge .skip_draw_top_pipe_left_pixel
	str r12 r2
	.next_pixel_pipe_top_left
		str r13 r1
		str r14 r0
		
		inc r1
		cmp r1 r9
		brh notzero .next_pixel_pipe_top_left
	
	mov r8 r1
	adi r1 10
	.skip_draw_top_pipe_left_pixel
	
	inc r2
	cmp r2 r9
	brh ge .skip_draw_top_pipe_right_pixel
	str r12 r2
	.next_pixel_pipe_top_right
		str r13 r1
		str r14 r0
		
		inc r1
		cmp r1 r9
		brh notzero .next_pixel_pipe_top_right
	
	.skip_draw_top_pipe_right_pixel
	
	mov r7 r7
	brh notzero .next_pipe

ret


.update_score
ldi r1 show_number
str r1 r3
ret


.movement
add r4 r5 r4
add r5 r6 r5
ret


.move_pipes
ldi r2 10
ldi r7 251
.move_next_pipe
	dec r2
	lod r2 r1
	dec r1
	cmp r7 r1
	brh notzero .continue_move_pipe
	adi r1 40
	inc r3
	.continue_move_pipe
	str r2 r1
	dec r2
	brh notzero .move_next_pipe

ret

.collisions
ldi r1 8
ldi r8 2
ldi r9 10
.next_pipe_range
	dec r1
	lod r1 r7
	cmp r9 r7
	brh lt .skip_pipe_range
	cmp r8 r7
	brh ge .skip_pipe_range
	jmp .exit_pipe_range
	.skip_pipe_range
	dec r1
	brh notzero .next_pipe_range
mov r0 r2
ret
.exit_pipe_range

mov r4 r9
rsh r9 r9
rsh r9 r9
rsh r9 r9

dec r1
lod r1 r1
cmp r9 r1
brh ge .skip_collision_bottom_pipe
ldi r2 1
mov r2 r2
ret
.skip_collision_bottom_pipe
adi r1 9
cmp r9 r1
brh lt .skip_collision_top_pipe
ldi r2 2
mov r2 r2
ret
.skip_collision_top_pipe

mov r0 r2
ret


.lose
cal .update_score
ldi r15 clear_chars_buffer
str r15 r0
ldi r15 write_char
ldi r1 "Y"
str r15 r1
ldi r1 "o"
str r15 r1
ldi r1 "u"
str r15 r1
ldi r1 " "
str r15 r1
ldi r1 "l"
str r15 r1
ldi r1 "o"
str r15 r1
ldi r1 "s"
str r15 r1
ldi r1 "t"
str r15 r1
ldi r1 "!"
str r15 r1
ldi r1 " "
str r15 r1
ldi r15 buffer_chars
str r15 r0
hlt


.win
cal .update_score
ldi r15 clear_chars_buffer
str r15 r0
ldi r15 write_char
ldi r1 "Y"
str r15 r1
ldi r1 "o"
str r15 r1
ldi r1 "u"
str r15 r1
ldi r1 " "
str r15 r1
ldi r1 "w"
str r15 r1
ldi r1 "i"
str r15 r1
ldi r1 "n"
str r15 r1
ldi r1 "!"
str r15 r1
ldi r1 " "
str r15 r1
ldi r1 " "
str r15 r1
ldi r15 buffer_chars
str r15 r0
hlt