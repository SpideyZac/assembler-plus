// Bresenham line drawing algorithm by zPippo

define base_port_addr 248
define pixel_x_port -8
define pixel_y_port -7
define draw_pixel_port -6
define buffer_screen_port -3
define clear_screen_buffer_port -2
define rng_port 6

ldi r15 base_port_addr
ldi r13 50
.draw_next_line
	str r15 r0 clear_screen_buffer_port
	cal .draw_line
	str r15 r0 buffer_screen_port
	
	dec r13
	brh notzero .draw_next_line
	
hlt


.draw_line
lod r15 r1 rng_port
lod r15 r2 rng_port
lod r15 r3 rng_port
lod r15 r4 rng_port
ldi r5 31
and r1 r5 r1 // X 1
and r2 r5 r2 // Y 1
and r3 r5 r3 // X 2
and r4 r5 r4 // Y 2

sub r1 r3 r5
sub r2 r4 r6

ldi r7 128
cmp r5 r7
brh lt .skip_invert_dx
sub r0 r5 r5
.skip_invert_dx
cmp r6 r7
brh lt .skip_invert_dy
sub r0 r6 r6
.skip_invert_dy

mov r5 r11 // max(dx, dy) = dx
cmp r5 r6
brh ge .skip_set_max_to_dy
mov r6 r11 // max(dx, dy) = dy
.skip_set_max_to_dy

ldi r7 -1 // sx = -1
inc r3
cmp r1 r3
brh ge .skip_set_sx_to_1 
ldi r7 1 // sx = 1
.skip_set_sx_to_1
dec r3
ldi r8 -1 // sy = -1
inc r4
cmp r2 r4
brh ge .skip_set_sy_to_1 
ldi r8 1 // sy = 1
.skip_set_sy_to_1
dec r4

sub r0 r6 r6 // dy = -dy

add r5 r6 r9 // error = dx - dy

ldi r12 0
.plot_next_pixel
	cmp r12 r11
	brh zero .exit_draw_pixel
	str r15 r1 pixel_x_port
	str r15 r2 pixel_y_port
	str r15 r0 draw_pixel_port
	
	lsh r9 r10 // error2 = 2 * error
	
	adi r10 -128
	adi r6 -128
	cmp r10 r6 // if error2 > -dy
	brh lt .skip_add_x
	add r9 r6 r9 // error -= dy
	add r1 r7 r1 // x += sx
	.skip_add_x
	adi r6 128
	
	adi r5 -128
	cmp r10 r5 // if error2 < dx
	brh ge .skip_add_y
	add r9 r5 r9 // error += dx
	add r2 r8 r2 // y += sy
	.skip_add_y
	adi r10 128
	adi r5 128
	
	inc r12
	jmp .plot_next_pixel

.exit_draw_pixel
str r15 r3 pixel_x_port
str r15 r4 pixel_y_port
str r15 r0 draw_pixel_port

ret

