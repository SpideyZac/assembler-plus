
// r15 pixel_x
// r14 rng
// r13 range
ldi r15 pixel_x
ldi r14 rng
ldi r13 31


//initialize display
//r1 r2
.listloop
	str r15 r1
	str r1 r1
	adi r1 1
	mov r1 r2
	.inner_loop
		dec r2
		str r15 r2 1
		str r15 r0 2
		brh ne .inner_loop
	str r15 r0 5	
	and r1 r13 r0
	brh ne .listloop
ldi r1 0

.shuffle
	mov r13 r2
	mov r13 r12
	rsh r12 r12
	.roll
		lod r14 r1
		and r1 r13 r1
		cmp r1 r2
		brh ge .roll
		cal .swap
		cmp r2 r12
		brh ne .c_shuffle
		mov r12 r13
		rsh r12 r12
		.c_shuffle
		dec r2
		brh ne .roll
ldi r13 31

jmp .bubble
hlt

//swap index r1 r2
.swap
	lod r1 r3 
	lod r2 r4
	cmp r3 r4
	str r1 r4
	str r2 r3
	brh lt .less
	jmp .more
	.less
		str r15 r4 1
		str r15 r1
		str r15 r0 2
		str r15 r2 
		str r15 r0 3
		dec r4 
		cmp r4 r3
		brh ne .less
		jmp .c_swap
	.more
		str r15 r3 1
		str r15 r1
		str r15 r0 3
		str r15 r2 
		str r15 r0 2
		dec r3
		cmp r3 r4
		brh ne .more
		jmp .c_swap
	.c_swap
	str r15 r0 5
	ret

.bubble
	mov r13 r12
	.o_b
		ldi r11 0
		ldi r1 0
		.i_b
			mov r1 r2 
			inc r2
			lod r1 r3
			lod r2 r4
			cmp r3 r4
			brh lt .no_swap
			cal .swap
			ldi r11 1
			.no_swap
			inc r1
			cmp r1 r12
			brh ne .i_b
		cmp r11 r0
		brh eq .exit
		dec r12
		brh ne .o_b
.exit
hlt
		
		
		