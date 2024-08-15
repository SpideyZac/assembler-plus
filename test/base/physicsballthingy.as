// Physics Ball thingy
// Thanks to Sebastian/Seb
// For designing the
// Fixed Point Arithmetic stuff

// r1 Ball Y High
// r2 Ball Y Low
// r3 Temp
// r4 Temp High
// r5 Temp Low
// r6 Ball Y Velocity High
// r7 Ball Y Velocity Low
// r8 Temp
// r9 Temp
// r10 Temp
// r11 Temp
// r12 Temp
// r13 Temp
// r14 Ball X
// r15 240

// Temp means that every function call
// could potentially change those registers

// the implementation makes use of
// properties from
// Newtons second law of motion
// To make the ball have gravity

define MG_HIGH -1
define MG_LOW 0

.init
	ldi r1 15
	ldi r14 15
	ldi r6 0

.physicsball
	cmp r1 r0
	brh eq .dosmthidk
	cal .fneg
.dosmthidk
	ldi r15 255
	cmp r1 r15
	brh eq .stop
	ldi r4 MG_HIGH
	ldi r5 MG_LOW
	cal .fsub
	cal .fadd

.drawball
	ldi r15 240
	str r15 r0 6
	str r15 r14
	str r15 r1 1
	str r15 r0 2
	str r15 r0 5
	jmp .physicsball

.stop
	ldi r1 0
	ldi r15 240
	str r15 r0 6
	str r15 r14
	str r15 r1 1
	str r15 r0 2
	str r15 r0 5
	hlt

.fneg
	sub r0 r6 r6
	ret

// r1 8-Bits high first value
// r2 8-Bits low first value
// r4 second high value
// r5 second low value
// r1 and r2 are the return value
.fadd
	add r7 r2 r2
	brh nc .addskip
	inc r1
.addskip
	add r1 r6 r1
	ret

// uses r6 and r7 instead of r1 and r2
.fsub
	sub r4 r6 r6
	brh c .subskip
	dec r7
.subskip
	sub r7 r5 r7
	ret