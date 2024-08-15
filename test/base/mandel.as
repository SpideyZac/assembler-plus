// r1 - scratch
// r2 - scratch
// r3 - scratch

// r4 - x (real)
// r5 - yi (imag)
// r6 - screen loop x
// r7 - screen loop y

// r8 - mult input A
// r9 - mult input B
// r10 - mult out high byte
// r11 - mult out low byte

// r12 - x^2-y^2 (real)
// r13 - 2xyi (imag)
// r14 - |real|+|imag|

// r15 - I/O

define pixel_x_port -8
define pixel_y_port -7
define draw_pixel_port -6
define clear_pixel_port -5
define load_pixel_port -4
define buffer_screen_port -3
define controller_input_port 7

  ldi r15 buffer_chars

  ldi r6 32
.screen_x_loop
  adi r6 -1

  ldi r7 32
.screen_y_loop
  adi r7 -1
  
  str r15 r6 pixel_x_port
  str r15 r7 pixel_y_port
  str r15 r0 draw_pixel_port
  str r15 r0 buffer_screen_port

  cal .screen_to_graph

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  cal .apply_brot_iter
  ldi r1 192 // 11000000 mask for >= 2
  and r1 r14 r0
  brh ne .bounds_exceeded

  jmp .max_iter_reached
  
.bounds_exceeded
  str r15 r6 pixel_x_port
  str r15 r7 pixel_y_port
  str r15 r0 clear_pixel_port
  str r15 r0 buffer_screen_port
.max_iter_reached

  cmp r7 r0
  brh ne .screen_y_loop

  cmp r6 r0
  brh ne .screen_x_loop

hlt

// waits until any button pressed
// then continues only after released
// uses r3
.breakpoint
  lod r15 r3 controller_input_port
  cmp r3 r0
  brh eq .breakpoint
.waiting_for_release
  lod r15 r3 controller_input_port
  cmp r3 r0
  brh ne .waiting_for_release
  ret

// .apply_brot_iter by pseudogravity
// one mandelbrot iteration
// r4/r5 hold previous iter
// r6/r7 hold screen coords
// r12/r13 output of x^2
// r4/r5 will hold x^2 + c
// r14 distance of point
.apply_brot_iter
  // x^2
  mov r4 r8
  mov r4 r9
  cal .mul_3_5_fixed_point
  mov r11 r12

  // -y^2
  mov r5 r8
  mov r5 r9
  cal .mul_3_5_fixed_point
  sub r12 r11 r12

  // 2xyi
  mov r4 r8
  mov r5 r9
  cal .mul_3_5_fixed_point
  lsh r11 r13

  // add c from graph
  cal .screen_to_graph
  add r12 r4 r4
  add r13 r5 r5

  // find distance of point from origin
  // calc |x|+|y|
  ldi r1 128
  mov r4 r2
  and r2 r1 r0
  brh eq .no_negate_real
  sub r0 r2 r2
.no_negate_real
  mov r2 r14
  mov r5 r2
  and r2 r1 r0
  brh eq .no_negate_imag
  sub r0 r2 r2
.no_negate_imag
  add r2 r14 r14

  ret


.screen_to_graph
  // screen to graph coords
  mov r6 r4
  mov r7 r5
  // mult by 2
  lsh r4 r4
  lsh r5 r5
  // shift by (-1.5,-1)
  adi r4 -48
  adi r5 -32
  ret


// .mul_3_5_fixed_point by pseudogravity
// 5 trailing bits signed fixed point multiplication
// r8 * r9 -> r11
.mul_3_5_fixed_point
  cal .mul8_8_16_signed
  rsh r11 r11
  rsh r11 r11
  rsh r11 r11
  rsh r11 r11
  ldi r1 1
  and r11 r1 r2 // test for rounding
  rsh r11 r11
  mov r10 r3
  lsh r3 r3
  lsh r3 r3
  lsh r3 r3
  add r3 r11 r11
  add r2 r11 r11
  // optional
  // cal .signed_rshift_r10
  // cal .signed_rshift_r10
  // cal .signed_rshift_r10
  // cal .signed_rshift_r10
  // cal .signed_rshift_r10
  ret


// signed rsh of r4
.signed_rshift_r4
  ldi r1 128             // 0x10000000
  and r4 r1 r0
  rsh r4 r4
  brh eq .no_leading_one_r4
  add r4 r1 r4
.no_leading_one_r4
  ret

// signed rsh of r5
.signed_rshift_r5
  ldi r1 128             // 0x10000000
  and r5 r1 r0
  rsh r5 r5
  brh eq .no_leading_one_r5
  add r5 r1 r5
.no_leading_one_r5
  ret

// signed rsh of r10
.signed_rshift_r10
  ldi r1 128             // 0x10000000
  and r10 r1 r0
  rsh r10 r10
  brh eq .no_leading_one_r10
  add r10 r1 r10
.no_leading_one_r10
  ret

// signed rsh of r11
.signed_rshift_r11
  ldi r1 128             // 0x10000000
  and r11 r1 r0
  rsh r11 r11
  brh eq .no_leading_one_r11
  add r11 r1 r11
.no_leading_one_r11
  ret


// 16 bit signed rsh of r10/r11
.signed_rshift_r10_r11
  rsh r11 r11
  ldi r1 128             // 0x10000000
  ldi r2 1
  and r10 r2 r0
  brh eq .no_shift_across
  add r11 r1 r11
.no_shift_across
  cal .signed_rshift_r10
  ret


// .mul8_8_16_signed by pseudogravity
.mul8_8_16_signed
  ldi r1 128             // 0x10000000
  cmp r8 r1
  brh ge .negate_A       // branch if A < 0
  cmp r9 r1
  brh ge .negate_B       // branch if B < 0
  cal .mul8_8_16         // return A * B
  ret
.negate_A                // A < 0
  sub r0 r8 r8           // A' = -A
  cmp r9 r1              
  brh ge .negate_AB      // branch if B < 0
  jmp .negate_res        // A < 0 && B >= 0
.negate_AB               // A < 0 && B < 0
  sub r0 r9 r9           // B' = -B
  cal .mul8_8_16         // return -A * -B
  ret
.negate_B                // A >= 0 && B < 0
  sub r0 r9 r9           // B' = -B
.negate_res              // result is negative
  cal .mul8_8_16         // compute either -A*B or A*-B
  not r10 r10            // 2's complement and return
  not r11 r11
  adi r11 1
  brh lt .negate_res_no_carry
  adi r10 1
.negate_res_no_carry
  ret


// .mul8_8_16 by Sebastian/Seb
// multiply 8 bit inputs and output in 16 bit in r10 and r11 (high byte, low byte respectively)
// r8 = A, r9 = B, r10 = high byte, r11 = low byte
// r1, r2, r3 are used for the algo
// r1, r2, r3, r8, r9, r10, r11 are changed
.mul8_8_16
  ldi r1 1  // mask for LSB
  cmp r8 r9 // skip swap if r8 >= r9 (r9 <= r8)
  brh ge .skip_swap
  xor r8 r9 r8 // swap r8, r9
  xor r8 r9 r9
  xor r8 r9 r8
.skip_swap
  xor r10 r10 r10 // reset r10, r11, r3
  xor r11 r11 r11
  xor r3 r3 r3
.mul_loop // mul loop
  and r9 r1 r0        // test for LSB
  brh z  .skip_add    // skip addition if zero
  add r10 r3 r10      // add r3 to r10 (high bytes of A and output)
  add r11 r8 r11      // add r8 to r11 (low bytes of A and output)
  brh nc .skip_add    // skip carry correction if no carry
  inc r10             // carry correction; increment r10 (high byte of output)
.skip_add
  rsh r9 r9           // shift down r9 to check next bit
  lsh r3 r3           // shift up r3 (high byte of A)
  lsh r8 r8           // shift up r8 (low byte of A)
  brh nc .skip_carry  // skip carry correction if no carry
  inc r3              // carry correction; increment r3 (high byte of A)
.skip_carry
  cmp r9 r0           // if B is non-zero, keep looping
  brh ne .mul_loop
  // if B is 0, exit loop (nothing left to multiply)
  ret // return from sub-routine