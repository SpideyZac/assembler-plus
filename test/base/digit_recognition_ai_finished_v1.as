//
// 
// 85% Accuracy digit recognition AI by
// 		Sloimay, Ketari & ECanDo
// It took over 70 cumulated hours of work
//
// - Press DPad directions to move your
//   brush around
// - Press B to toggle drawing on or off
// - Press A to clear the screen
// - Once you're done drawing a digit,
//   press Start to start the digit
//	 recognition. It'll be shown in the
//	 digit display at the top of the
// 	 screen once it's done!
//	 (It'll take around a million
//	 instructions so strap in lol)
//
//
// Ram 0: Controller input at the end
//		  of the loop
// Ram 1: Brush below pixel data
// Ram 2: Drawing toggle
// Ram 3: Fn Predict neuron for loop neur id
// Ram 4: Fn Predict y for loop y
// Ram 5: Fn Predict x for loop x
// Ram 6: Fn Predict x for loop max x
// Ram 16: Curr batch
// Ram 17: Idx within curr batch
// Ram 18: Idx within curr batch byte
// Ram [19; 32]: Byte weight 1st layer load 
// Ram [33; 47]: Main loop reg save
// Ram [50; 50+NEURON_COUNT-1]: Hidden Layer 1 accs
// Ram [70; 74]: Compressed byte decode
// Ram 75: Temp weight holding cell
// Ram [80; 99]: 2-byte uints output layer
// Ram 100: Output Neuron loop neuron
// Ram 101: Hidden layer loop neuron
// Ram [110; 139]: HL Layer Compressed Weights
// Ram [150; 165]: Output layer biases (only
// 				  first 10 are used)
// Ram [200; 209]: Random debug bytes

define base_port_addr 248
define pixel_x_port -8
define pixel_y_port -7
define draw_pixel_port -6
define clear_pixel_port -5
define load_pixel_port -4
define buffer_screen_port -3
define clear_screen_buffer_port -2
define write_char_port -1
define buffer_chars_port 0
define clear_chars_buffer_port 1
define show_number_port 2
define clear_number_port 3
define signed_mode_port 4
define unsigned_mode_port 5
define rng_port 6
define controller_input_port 7

define CURR_BATCH_IDX_ADDR 16
define BYTE_IDX_WITHIN_CURR_BATCH_ADDR 17
define IDX_WITHIN_BBYTE_ADDR 18

define FIRST_LAYER_BYTE_LOAD_ADDR 19
define HIDDEN_LAYER_ACCS_ADDR 50
define COMPRESSED_BYTE_DECODE_ADDR 70

define WEIGHT_HOLDING_CELL 75

define OUTPUT_LAYER_BASE_PTR 80
define OUTPUT_NEURON_DECR_ADDR 100
define HL_NEURON_DECR_ADDR 101
define HL_WEIGHTS_PTR 110

define OL_BIASES_PTR 150

define DEBUG_ADDR 200


define NEURON_COUNT 5
define LAYER1_BATCH_SIZE 14

// Reset RAM bytes: [0; 239]
ldi r1 241
.reset_ram_loop
	dec r1
	str r1 r0 -1
	brh nz .reset_ram_loop

// Start brush XY
ldi r1 16
ldi r2 16




// # r1 mouseX
// # r2 mouseY
.main_loop


	// # Get the current inputs
	// # and check for rising edges
	// r14 should be controller inputs
	// pooled but unchanged
	ldi r15 controller_input
	lod r15 r14
	// r13 is current controller inputs
	mov r14 r13
	// r12 is controller inputs last loop
	lod r0 r12 0
	// r8 is just the 1 mask
	ldi r8 1

	// r11 and r10 are the current and last
	// bits

	// Presetup dx dy
	ldi r6 0
	ldi r7 0
	// Handle left
	and r13 r8 r11
	and r12 r8 r10
	cmp r10 r11
	brh ge .not_left_rising_edge
		ldi r6 -1
		cal .fn_move_brush
	.not_left_rising_edge

	// Handle down
	cal .main_loop__shift_and_cmp
	brh ge .not_down_rising_edge
		ldi r7 -1
		cal .fn_move_brush
	.not_down_rising_edge

	// Handle right
	cal .main_loop__shift_and_cmp
	brh ge .not_right_rising_edge
		ldi r6 1
		cal .fn_move_brush
	.not_right_rising_edge

	// Handle up
	cal .main_loop__shift_and_cmp
	brh ge .not_up_rising_edge
		ldi r7 1
		cal .fn_move_brush
	.not_up_rising_edge



	// Handle B
	cal .main_loop__shift_and_cmp
	brh ge .not_b_rising_edge
		// Toggle draw
		lod r0 r7 2
		ldi r6 1
		sub r6 r7 r7
		str r0 r7 2
	.not_b_rising_edge

	// Handle A
	cal .main_loop__shift_and_cmp
	brh ge .not_a_rising_edge
		// Clear screen
		ldi r7 base_port_addr
		str r7 r0 clear_screen_buffer_port
		str r7 r0 buffer_screen_port
		// Reset below pixel mem
		str r0 r0 1
	.not_a_rising_edge

	// Handle Select
	cal .main_loop__shift_and_cmp
	brh ge .not_select_rising_edge
		// not hing! ! !
	.not_select_rising_edge

	// Handle Start
	cal .main_loop__shift_and_cmp
	brh ge .not_start_rising_edge
		// ## Predict!!
		// # Save all our current regs
		// # (Except r4 as it's our base idx)
		// Get base idx
		ldi r4 40
		// Save
		// Registers to save have been
		// chosen carefully considering
		// the current state of the
		// main loop's code. Make sure
		// that if changed, you look here
		// and add some registers back if
		// needed
		str r4 r1 -7
		str r4 r2 -6
		//str r4 r3 -5
		//str r4 r5 -3
		//str r4 r6 -2
		//str r4 r7 -1
		//str r4 r8 0
		//str r4 r9 1
		//str r4 r10 2
		//str r4 r11 3
		//str r4 r12 4
		//str r4 r13 5
		str r4 r14 6
		//str r4 r15 7
		cal .fn_switch_pix_at_brush
		// Predict
		cal .fn_predict
		// # Restore regs
		// Get base idx
		ldi r4 40
		// Restore
		lod r4 r1 -7
		lod r4 r2 -6
		//lod r4 r3 -5
		//lod r4 r5 -3
		//lod r4 r6 -2
		//lod r4 r7 -1
		//lod r4 r8 0
		//lod r4 r9 1
		//lod r4 r10 2
		//lod r4 r11 3
		//lod r4 r12 4
		//lod r4 r13 5
		lod r4 r14 6
		//lod r4 r15 7
		cal .fn_switch_pix_at_brush
	.not_start_rising_edge

	// ## At the end of the loop, store
	// ## the inputs
	str r0 r14 0

jmp .main_loop


.main_loop__shift_and_cmp
	rsh r13 r13
	rsh r12 r12
	and r13 r8 r11
	and r12 r8 r10
	cmp r10 r11
ret





// # r6 is deltaX
// # r7 is deltaY
// # r5 is _oldPixel
// # r4 is _temp
.fn_move_brush__temp_fn_draw
	ldi r4 base_port_addr
	str r4 r1 pixel_x_port
	str r4 r2 pixel_y_port
	str r4 r0 draw_pixel_port
ret

.fn_move_brush
	
	// # Set pixel to what it's
	// # meant to be
	// Put the old pixel
	// state into r5
	lod r0 r5 1
	// If old pixel state == 1 set pix
	ldi r4 1
	cmp r5 r4
	ldi r4 base_port_addr
	brh ne .fn_move_brush__old_pix_ne1
		str r4 r0 draw_pixel_port
	.fn_move_brush__old_pix_ne1
	// If old pixel state == 0 clear pix
	brh eq .fn_move_brush__old_pix_eq1
		str r4 r0 clear_pixel_port
	.fn_move_brush__old_pix_eq1
	
	// Move brush
	add r1 r6 r1
	add r2 r7 r2
	
	// # Bounds checking
	// -x
	ldi r4 3
	cmp r1 r4
	brh ge .fn_move_brush__in_mx_bound
		ldi r1 3
	.fn_move_brush__in_mx_bound
	// -y -- use r4 already equal to 3
	cmp r2 r4
	brh ge .fn_move_brush__in_my_bound
		ldi r2 3
	.fn_move_brush__in_my_bound
	// +x
	ldi r4 29
	cmp r1 r4
	brh lt .fn_move_brush__in_px_bound
		ldi r1 28
	.fn_move_brush__in_px_bound
	// +y -- use r4 already equal to 29
	cmp r2 r4
	brh lt .fn_move_brush__in_py_bound
		ldi r2 28
	.fn_move_brush__in_py_bound
	
	// # If the drawing toggle is on
	// # ... draw
	// r4 is the toggle state
	// r5 is 1
	lod r0 r4 2
	ldi r5 1
	cmp r4 r5
	brh ne .fn_move_brush__not_draw
		
		adi r1 -1
		cal .fn_move_brush__temp_fn_draw
		adi r1 2
		cal .fn_move_brush__temp_fn_draw
		adi r1 -1
		cal .fn_move_brush__temp_fn_draw
		adi r2 1
		cal .fn_move_brush__temp_fn_draw
		adi r2 -2
		cal .fn_move_brush__temp_fn_draw
		
		adi r2 1
		
	.fn_move_brush__not_draw
	
	
	// Get the current pixel state and
	// store it
	ldi r4 base_port_addr
	str r4 r1 pixel_x_port
	str r4 r2 pixel_y_port
	// r5 is the current pixel data
	lod r4 r5 load_pixel_port
	str r0 r5 1
	
	// # Set pixel to the inverse
	// # what it's meant to be
	// If current pixel state == 1 clear pix
	ldi r4 1
	cmp r5 r4
	ldi r4 base_port_addr
	brh ne .fn_move_brush__curr_pix_ne1
		str r4 r0 clear_pixel_port
	.fn_move_brush__curr_pix_ne1
	// If current pixel state == 0 set pix
	brh eq .fn_move_brush__curr_pix_eq1
		str r4 r0 draw_pixel_port
	.fn_move_brush__curr_pix_eq1
	
	// # Update screen
	str r4 r0 buffer_screen_port
	
ret


.fn_switch_pix_at_brush
	// Get pixel at brush
	ldi r3 base_port_addr
	str r3 r1 pixel_x_port
	str r3 r2 pixel_y_port
	lod r3 r4 load_pixel_port
	
	// Draw switched pixel state
	str r3 r0 clear_pixel_port
	adi r4 0
	brh nz .fn_switch_pix_at_brush__1
		str r3 r0 draw_pixel_port
	.fn_switch_pix_at_brush__1
	
	str r3 r0 buffer_screen_port
ret





.fn_predict
	
	// Clear number display
	ldi r3 base_port_addr
	str r3 r0 clear_number_port
	
	// Batch reset
	ldi r1 CURR_BATCH_IDX_ADDR
	str r1 r0
	ldi r1 BYTE_IDX_WITHIN_CURR_BATCH_ADDR
	str r1 r0
	ldi r1 IDX_WITHIN_BBYTE_ADDR
	str r1 r0

	// Set current neuron to 0
	ldi r1 0
	str r0 r1 3
	
	// Profit off of ldi r1 0
	// to load the first batch
	cal .fn_load_batch
	
	.fn_predict__neuron_for
		
		// At the start of the Y loop
		// set Y to 28 (the top of the
		// canvas)
		ldi r1 28
		str r0 r1 4
		
		// Reset acc to 0
		ldi r1 HIDDEN_LAYER_ACCS_ADDR
		lod r0 r2 3
		add r1 r2 r1
		str r1 r0
		
		.fn_predict__y_for
		
			cal .fn_predict__fn_setup_x
			
			// Get max X = 32 - start_x (r1)
			lod r0 r1 5
			ldi r2 32
			sub r2 r1 r2
			str r0 r2 6
			
			// # Start X iterating
			.fn_predict__x_for
				
				// Oval testing code
				//ldi r4 base_port_addr
				//lod r0 r1 5
				//str r4 r1 pixel_x_port
				//lod r0 r1 4
				//str r4 r1 pixel_y_port
				//str r4 r0 draw_pixel_port
				//str r4 r0 buffer_screen_port
				
				// Get current weight!!
				cal .fn_predict__fn_get_weight
				ldi r2 WEIGHT_HOLDING_CELL
				str r2 r1
				
				// Get the pixel input data
				// in r1
				// Can use all regs here
				// as long as I don't need
				// persistent data thru
				// loops
				ldi r3 base_port_addr
				lod r0 r4 5 // x
				str r3 r4 pixel_x_port
				lod r0 r4 4 // y
				str r3 r4 pixel_y_port
				lod r3 r1 load_pixel_port					
				
				
				// # Mult weight and pix
				// # (result in r1)
				// turn pix into an AND
				// mask, so if 0 => 00000000
				//          if 1 => 11111111
				not r1 r1
				adi r1 1
				
				// AND-ing at this point
				// is mult
				ldi r2 WEIGHT_HOLDING_CELL
				lod r2 r2
				and r2 r1 r1
				
				// Add mult res into acc
				ldi r2 HIDDEN_LAYER_ACCS_ADDR
				lod r0 r3 3
				add r2 r3 r2 // r2 = acc ptr
				lod r2 r3 // r3 = acc val
				add r3 r1 r3 // r3 = acc + mult res
				str r2 r3 // store acc back
				
				// # End of loop
				// Update idx within
				// curr batch byte
				ldi r1 IDX_WITHIN_BBYTE_ADDR
				lod r1 r1
				inc r1
				ldi r2 5
				cmp r1 r2
				ldi r2 BYTE_IDX_WITHIN_CURR_BATCH_ADDR
				lod r2 r3
				brh ne .fn_predict__not_bbyte_limit
					ldi r1 0
					// Inc byte idx within curr batch
					inc r3
					str r2 r3
				.fn_predict__not_bbyte_limit
				ldi r2 IDX_WITHIN_BBYTE_ADDR
				str r2 r1
				
				// If byte within batch >= l1_batch_size
				ldi r2 LAYER1_BATCH_SIZE
				cmp r3 r2
				brh ne .fn_predict__not_batch_end
					// Reset idx within curr batch
					ldi r1 BYTE_IDX_WITHIN_CURR_BATCH_ADDR
					str r1 r0
					// Inc curr batch
					ldi r1 CURR_BATCH_IDX_ADDR
					lod r1 r2
					inc r2
					str r1 r2
					// Load next batch
					// Destructive of every reg
					mov r2 r1
					cal .fn_load_batch
				.fn_predict__not_batch_end
				
				// End for loop logic
				lod r0 r1 5 // Get curr X
				inc r1
				str r0 r1 5
				// Check if we are at the end of
				// the row
				lod r0 r2 6 // Get max x
				cmp r1 r2
				brh eq .fn_predict__end_x_for
				jmp .fn_predict__x_for
			.fn_predict__end_x_for
		
			lod r0 r1 4
			dec r1
			str r0 r1 4
			// Check if we are at the bottom
			ldi r2 2
			cmp r1 r2
			brh eq .fn_predict__end_y_for
			jmp .fn_predict__y_for
		.fn_predict__end_y_for
		
		
		lod r0 r1 3
		inc r1
		str r0 r1 3
		// Check if we are done with neurons
		ldi r2 NEURON_COUNT
		cmp r1 r2
		brh eq .fn_predict__end_neuron_for
		jmp .fn_predict__neuron_for
	.fn_predict__end_neuron_for
    // # By this point, each of the
	// # neurons been accumulated
	// # (base RAM addr at 50)
	
	// # Apply biases
	// [ -1, 1, -1, 2, 1 ]
	ldi r2 HIDDEN_LAYER_ACCS_ADDR
	lod r2 r1 0
	adi r1 -1
	str r2 r1 0
	lod r2 r1 1
	adi r1 1
	str r2 r1 1
	lod r2 r1 2
	adi r1 -1
	str r2 r1 2
	lod r2 r1 3
	adi r1 2
	str r2 r1 3
	lod r2 r1 4
	adi r1 1
	str r2 r1 4
	
	// # Load HL weights
	ldi r1 HL_WEIGHTS_PTR
	adi r1 5
	
	ldi r3 255
    ldi r4 62
    ldi r5 14
    ldi r6 240
    ldi r7 16
    ldi r8 207
    ldi r9 33
    ldi r10 223
    ldi r11 227
    ldi r12 44
    ldi r13 34
    ldi r14 223
    ldi r15 237
	cal .fn_predict__fn_load_hl_weights

    ldi r3 178
    ldi r4 226
    ldi r5 211
    ldi r6 224
    ldi r7 240
    ldi r8 29
    ldi r9 78
    ldi r10 228
    ldi r11 251
    ldi r12 226
    ldi r13 33
    ldi r14 221
	cal .fn_predict__fn_load_hl_weights
	
	
	// # Activations + Mults
	// # Now that I already have imported
	// # every weight, I can every
	// # register I'd want
	// # r15 = output neuron decr idx
	// # r14 = hl neuron idx
	// # r13 = weight idx
	// # r12 = temp
	ldi r15 20
	ldi r13 0
	.fn_predict__loop_output_neuron
		ldi r14 NEURON_COUNT
		.fn_predict__loop_hl_neuron
			
			// Upper 7 bits are the
			// weight array idx
			rsh r13 r2
			adi r2 HL_WEIGHTS_PTR
			lod r2 r2
			
			// r2 is now the weight
			// compressed byte
			// Objective is to get it
			// to r1
			ldi r12 15
			and r2 r12 r1
			ldi r12 1
			and r13 r12 r12
			brh z .fn_predict__dont_shift_hl_weight
				rsh r2 r1
				rsh r1 r1
				rsh r1 r1
				rsh r1 r1
			.fn_predict__dont_shift_hl_weight
			
			// Now we need the hl neuron value
			mov r14 r2
			adi r2 HIDDEN_LAYER_ACCS_ADDR
			lod r2 r2 -1
			
			// Mult, result in r4 & r5
			cal .fn_predict__fn_acti_mult
			
			// # Acc
			mov r15 r1
			adi r1 OUTPUT_LAYER_BASE_PTR
			// Load low and high byte
			lod r1 r2 -2
			lod r1 r3 -1
			// Add r4 & r5 to r2 & r3
			add r2 r4 r2
			brh nc .fn_predict__hl_loop_nc
				inc r3
			.fn_predict__hl_loop_nc
			add r3 r5 r3
			// Store the acc back
			str r1 r2 -2
			str r1 r3 -1
			
			// End hl logic loop
			inc r13
			dec r14
			brh nz .fn_predict__loop_hl_neuron
		// End output neuron logic loop
		adi r15 -2
		brh nz .fn_predict__loop_output_neuron
	
	// # Import biases by re-using the
	// # same that imports HL weights
	// # [-2 -1  1  2  3  2  0 -1 -4 -2]
	ldi r1 OL_BIASES_PTR
	adi r1 5
	
	ldi r3 -2
	ldi r4 -1
	ldi r5 1
	ldi r6 2
	ldi r7 3
	ldi r8 2
	ldi r9 0
	ldi r10 -1
	ldi r11 -4
	ldi r12 -2
	cal .fn_predict__fn_load_hl_weights
	
	// # Add biases and figure out which
	// # output is the bigger one in the
	// # same loop
	// # r1 = loop decr (also neuron (idx+1)*2)
	// # r2 = output neuron low
	// # r3 = output neuron high
	// # r4 = temp
	// # r5 = bias
	// # r6 = current digit iterated over
	// # r7 = running low byte max
	// # r8 = running high byte max
	// # r9 = running best digit
	// # r10 = bool if current >= running
	ldi r1 20
	ldi r4 0
	ldi r7 0
	ldi r8 0
	ldi r9 9
	.fn_predict__choosing_loop
		
		// Load neuron
		mov r1 r4
		adi r4 OUTPUT_LAYER_BASE_PTR
		lod r4 r2 -2
		lod r4 r3 -1
		
		// Load bias
		mov r1 r4
		rsh r4 r4 // r1 is twice too big fix
		mov r4 r6
		dec r6
		adi r4 OL_BIASES_PTR
		lod r4 r5 -1
		
		// Add
		// add low
		add r2 r5 r2
		// handle carry
		brh nc .fn_predict__cl__nc1
			inc r3
		.fn_predict__cl__nc1
		// If bias is negative I need to
		// add 255 to the high byte
		ldi r4 128
		cmp r5 r4
		brh lt .fn_predict__cl__negative_bias
			adi r3 255
		.fn_predict__cl__negative_bias
		
		//ldi r11 DEBUG_ADDR
		//add r11 r1 r11
		//str r11 r2 -2
		//str r11 r3 -1
		
		// Add to make every neuron for
		// sure positive but still keep
		// ordering
		adi r3 32
		
		ldi r10 0
		// if curr highb >= best highb
		cmp r3 r8
		brh lt .fn_predict__cl__lt
			
			// Make the hypothesis that
			// curr is greater
			ldi r10 1
			
			// Hypothesis can only be false
			// if curr highb == best highb &&
			//    curr lowb  <  best lowb
			// if curr highb == best highb
			cmp r3 r8
			brh ne .fn_predict__cl__gene
				// we have to compare
				// low bytes
				// if curr lowb < best lowb
				cmp r2 r7
				brh ge .fn_predict__cl__genege
					// Both high bytes are the
					// same but curr low byte
					// is smaller than best
					// low byte so our number
					// is actually smaller
					// Hypothesis false
					ldi r10 0
				.fn_predict__cl__genege
			.fn_predict__cl__gene
		.fn_predict__cl__lt
		
		// Set new best if found
		adi r10 0
		brh z .fn_predict__cl__didnt_find_new_best
			mov r2 r7
			mov r3 r8
			mov r6 r9
		.fn_predict__cl__didnt_find_new_best
	
		// End loop logic
		adi r1 -2
		brh nz .fn_predict__choosing_loop
	
	// # r9 is the recognized digit!!!!
	// # so display it!!!!!
	ldi r1 base_port_addr
	str r1 r9 show_number_port
	
	
ret




.fn_predict__fn_load_hl_weights
	str r1 r3 -5
	str r1 r4 -4
	str r1 r5 -3
	str r1 r6 -2
	str r1 r7 -1
	str r1 r8 0
	str r1 r9 1
	str r1 r10 2
	str r1 r11 3
	str r1 r12 4
	str r1 r13 5
	str r1 r14 6
	str r1 r15 7
	
	adi r1 13
ret



// ## Function only meant to be
// ## called from within fn_predict
.fn_predict__fn_setup_x
	// # Get current Y in r1
	lod r0 r1 4
	// # Get X start in r1
	ldi r2 16
	cmp r1 r2
	// If y >= 16: pool_y = 31 - y
	brh lt .fn_predict__no_remap_y
		ldi r2 31
		sub r2 r1 r1
	.fn_predict__no_remap_y
	// -16 = -3 - 13
	adi r1 -16
	// At this point r1 is y but
	// remapped, will be inputted
	// in the LUT
	// VV LUT VV
	adi r1 3 // The first 3 entries are "5"
    brh nc .fn_predict__lut_y_not_12
    // Y in LUT == 12
        ldi r2 5
    .fn_predict__lut_y_not_12
    adi r1 3
    brh nc .fn_predict__lut_y_not_9
    // Y in LUT == 9
        ldi r2 6
    .fn_predict__lut_y_not_9
    adi r1 1
    brh nc .fn_predict__lut_y_not_6
    // Y in LUT == 6
        ldi r2 7
    .fn_predict__lut_y_not_6
    adi r1 2
    brh nc .fn_predict__lut_y_not_5
    // Y in LUT == 5
        ldi r2 8
    .fn_predict__lut_y_not_5
    adi r1 1
    brh nc .fn_predict__lut_y_not_3
    // Y in LUT == 3
        ldi r2 9
    .fn_predict__lut_y_not_3
    adi r1 1
    brh nc .fn_predict__lut_y_not_2
    // Y in LUT == 2
        ldi r2 10
    .fn_predict__lut_y_not_2
    adi r1 1
    brh nc .fn_predict__lut_y_not_1
    // Y in LUT == 1
        ldi r2 12
    .fn_predict__lut_y_not_1
    adi r1 1
    brh nc .fn_predict__lut_y_not_0
    // Y in LUT == 0
        ldi r2 14
    .fn_predict__lut_y_not_0
	str r0 r2 5
ret


// # Get and store weight in r1
.fn_predict__fn_get_weight

	// addr: FIRST_LAYER_BYTE_LOAD_ADDR
	//      BYTE_IDX_WITHIN_CURR_BATCH_ADDR
	//      IDX_WITHIN_BBYTE_ADDR
	//      COMPRESSED_BYTE_DECODE_ADDR
	// Get byte in r1
	// r1 = mem[byte_base + byte_ptr]
	ldi r1 FIRST_LAYER_BYTE_LOAD_ADDR
	ldi r2 BYTE_IDX_WITHIN_CURR_BATCH_ADDR
	lod r2 r2
	add r1 r2 r1
	lod r1 r1
	
	// ## Decode byte
	ldi r2 COMPRESSED_BYTE_DECODE_ADDR
	str r2 r0 0
	str r2 r0 1
	str r2 r0 2
	str r2 r0 3
	str r2 r0 4
	
	// # Bin to ternary
	// # r1 is the byte to decode
	// # r2 is the 8 counter
	// # r3 is temp
	// # r4 is carry
	// # r5 is the 5 counter
	// # r6 is the current digit value
	// # r7 current digit ptr
	ldi r2 0
	.fn_predict__fn_get_weight_8_loop
		
		// Get first carry (input bit)
		// (input & 1)
		// (The LUT numbers are inverted)
		ldi r3 1
		and r1 r3 r4
		rsh r1 r1
		
		ldi r5 0
		.fn_predict__fn_get_weight_5_loop
			
			// Get current digit byte
			ldi r3 COMPRESSED_BYTE_DECODE_ADDR
			add r3 r5 r7
			lod r7 r6
			
			// Double it (+ carry)
			add r6 r6 r6
			add r6 r4 r6
			// And give it to the next person
			// (Set next carry and do
			// overflow logic)
			ldi r4 0 // Carry if no overflow
			ldi r3 3
			cmp r6 r3
			brh lt .fn_pred__fn_gw__digit_no_overflow
				ldi r4 1 // Carry
				adi r6 -3 // Adjusting
			.fn_pred__fn_gw__digit_no_overflow
			str r7 r6 // Store digit back
			
			// End 5 loop logic
			adi r5 1
			ldi r3 5
			cmp r5 r3
			brh eq .fn_predict__fn_get_weight_end_5_loop
			jmp .fn_predict__fn_get_weight_5_loop
		.fn_predict__fn_get_weight_end_5_loop
		
		// End 8 loop logic
		adi r2 1
		ldi r3 8
		cmp r2 r3
		brh eq .fn_predict__fn_get_weight_end_8_loop
		jmp .fn_predict__fn_get_weight_8_loop
	.fn_predict__fn_get_weight_end_8_loop

	// # Every weight decoded in the
	// # range 0..=2
	// # Put correct weight in r1
	// # (with -1 shifting)
	ldi r1 IDX_WITHIN_BBYTE_ADDR
	lod r1 r1
	adi r1 COMPRESSED_BYTE_DECODE_ADDR 
	lod r1 r1
	adi r1 -1

ret


// # r1 is weight
// # r2 is neuron value
// # r3 is temp and then sign
// # Result in r4 and r5
// # r6 is another temp
// # r7 is loop counter
// # r8 is neuron val high byte
.fn_predict__fn_acti_mult
	
	ldi r4 0
	ldi r5 0
	ldi r8 0
	
	// # Apply relu activation to r2
	// # We do it here so it takes
	// # less instructions
	// # if r2 >= 128 (negative): r2 = 0
	ldi r3 128
	cmp r2 r3
	brh lt .fn_predict__fn_am__no_acti
		ldi r2 0
	.fn_predict__fn_am__no_acti
	
	// # Make r1 positive if needed
	// # if r1 >= 8 (negative): r1 *= -1
	// # No weight == -8 so we g
	ldi r3 8
	cmp r1 r3
	ldi r3 0 // Sign val if pos
	brh lt .fn_predict__fn_am__no_sign
		adi r1 240 // Sign extend r1
		not r1 r1
		adi r1 1
		ldi r3 1
	.fn_predict__fn_am__no_sign
	
	// # Perform mult
	ldi r7 3
	.fn_predict__fn_am__loop
	
		// Get if we add
		ldi r6 1
		and r1 r6 r6 // r6 is 1 if we add
		brh z .fn_predict__fn_am__loop_dont_add
			add r4 r2 r4 // Add low bytes
			brh nc .fn_predict__fn_am__loop_nc2
				// We have a carry in
				// the low byte addition
				inc r5
			.fn_predict__fn_am__loop_nc2
			add r5 r8 r5 // Add high bytes
		.fn_predict__fn_am__loop_dont_add
		

		// at the end shift neuron val
		// for next loop
		lsh r8 r8 // high byte
		lsh r2 r2 // low byte
		brh nc .fn_predict__fn_am__loop_nc1
			// There is a carry meaning
			// a "1" bit has been shifted
			// out. Put it inside the
			// high byte
			adi r8 1
		.fn_predict__fn_am__loop_nc1
		
		// Shift down the weight
		rsh r1 r1
		
		dec r7
	brh nz .fn_predict__fn_am__loop
	
	// Fix sign
	add r3 r0 r3
	brh z .fn_predict__fn_am__no_fix_sign
		not r4 r4
		not r5 r5
		adi r4 1
		brh nc .fn_predict__fn_am__fix_sign_nc
			inc r5
		.fn_predict__fn_am__fix_sign_nc
	.fn_predict__fn_am__no_fix_sign
	
	
ret




// # r1 is batch index
.fn_load_batch
	adi r1 -31
	adi r1 1
	brh nc .fn_load_batch__skip0
	    ldi r2 167
	    ldi r3 79
	    ldi r4 97
	    ldi r5 143
	    ldi r6 133
	    ldi r7 151
	    ldi r8 79
	    ldi r9 43
	.fn_load_batch__skip0
	adi r1 1
	brh nc .fn_load_batch__skip1
	    ldi r2 247
	    ldi r3 20
	    ldi r4 30
	    ldi r5 158
	    ldi r6 119
	    ldi r7 97
	    ldi r8 83
	    ldi r9 107
	    ldi r10 133
	    ldi r11 158
	    ldi r12 41
	    ldi r13 203
	    ldi r14 83
	    ldi r15 107
	.fn_load_batch__skip1
	adi r1 1
	brh nc .fn_load_batch__skip2
	    ldi r2 160
	    ldi r3 174
	    ldi r4 160
	    ldi r5 54
	    ldi r6 136
	    ldi r7 14
	    ldi r8 86
	    ldi r9 0
	    ldi r10 176
	    ldi r11 158
	    ldi r12 203
	    ldi r13 0
	    ldi r14 30
	    ldi r15 83
	.fn_load_batch__skip2
	adi r1 1
	brh nc .fn_load_batch__skip3
	    ldi r2 119
	    ldi r3 158
	    ldi r4 138
	    ldi r5 158
	    ldi r6 125
	    ldi r7 190
	    ldi r8 128
	    ldi r9 96
	    ldi r10 48
	    ldi r11 5
	    ldi r12 32
	    ldi r13 216
	    ldi r14 0
	    ldi r15 135
	.fn_load_batch__skip3
	adi r1 1
	brh nc .fn_load_batch__skip4
	    ldi r2 211
	    ldi r3 158
	    ldi r4 62
	    ldi r5 119
	    ldi r6 169
	    ldi r7 158
	    ldi r8 190
	    ldi r9 75
	    ldi r10 158
	    ldi r11 158
	    ldi r12 97
	    ldi r13 32
	    ldi r14 129
	    ldi r15 158
	.fn_load_batch__skip4
	adi r1 1
	brh nc .fn_load_batch__skip5
	    ldi r2 41
	    ldi r3 193
	    ldi r4 158
	    ldi r5 211
	    ldi r6 151
	    ldi r7 151
	    ldi r8 94
	    ldi r9 211
	    ldi r10 115
	    ldi r11 5
	    ldi r12 65
	    ldi r13 83
	    ldi r14 103
	    ldi r15 79
	.fn_load_batch__skip5
	adi r1 1
	brh nc .fn_load_batch__skip6
	    ldi r2 79
	    ldi r3 235
	    ldi r4 143
	    ldi r5 79
	    ldi r6 0
	    ldi r7 0
	    ldi r8 54
	    ldi r9 128
	    ldi r10 30
	    ldi r11 39
	    ldi r12 94
	    ldi r13 179
	    ldi r14 79
	    ldi r15 121
	.fn_load_batch__skip6
	adi r1 1
	brh nc .fn_load_batch__skip7
	    ldi r2 97
	    ldi r3 158
	    ldi r4 158
	    ldi r5 119
	    ldi r6 235
	    ldi r7 158
	    ldi r8 233
	    ldi r9 247
	    ldi r10 133
	    ldi r11 158
	    ldi r12 167
	    ldi r13 151
	    ldi r14 169
	    ldi r15 167
	.fn_load_batch__skip7
	adi r1 1
	brh nc .fn_load_batch__skip8
	    ldi r2 160
	    ldi r3 147
	    ldi r4 97
	    ldi r5 32
	    ldi r6 164
	    ldi r7 99
	    ldi r8 190
	    ldi r9 20
	    ldi r10 164
	    ldi r11 135
	    ldi r12 136
	    ldi r13 122
	    ldi r14 36
	    ldi r15 23
	.fn_load_batch__skip8
	adi r1 1
	brh nc .fn_load_batch__skip9
	    ldi r2 167
	    ldi r3 91
	    ldi r4 151
	    ldi r5 52
	    ldi r6 83
	    ldi r7 88
	    ldi r8 62
	    ldi r9 5
	    ldi r10 32
	    ldi r11 203
	    ldi r12 90
	    ldi r13 119
	    ldi r14 190
	    ldi r15 174
	.fn_load_batch__skip9
	adi r1 1
	brh nc .fn_load_batch__skip10
	    ldi r2 160
	    ldi r3 147
	    ldi r4 46
	    ldi r5 79
	    ldi r6 16
	    ldi r7 189
	    ldi r8 154
	    ldi r9 79
	    ldi r10 88
	    ldi r11 54
	    ldi r12 50
	    ldi r13 183
	    ldi r14 79
	    ldi r15 182
	.fn_load_batch__skip10
	adi r1 1
	brh nc .fn_load_batch__skip11
	    ldi r2 119
	    ldi r3 86
	    ldi r4 158
	    ldi r5 218
	    ldi r6 50
	    ldi r7 30
	    ldi r8 158
	    ldi r9 158
	    ldi r10 154
	    ldi r11 65
	    ldi r12 170
	    ldi r13 83
	    ldi r14 154
	    ldi r15 79
	.fn_load_batch__skip11
	adi r1 1
	brh nc .fn_load_batch__skip12
	    ldi r2 79
	    ldi r3 79
	    ldi r4 215
	    ldi r5 79
	    ldi r6 79
	    ldi r7 103
	    ldi r8 133
	    ldi r9 235
	    ldi r10 41
	    ldi r11 161
	    ldi r12 110
	    ldi r13 41
	    ldi r14 170
	    ldi r15 30
	.fn_load_batch__skip12
	adi r1 1
	brh nc .fn_load_batch__skip13
	    ldi r2 83
	    ldi r3 79
	    ldi r4 190
	    ldi r5 158
	    ldi r6 203
	    ldi r7 151
	    ldi r8 94
	    ldi r9 158
	    ldi r10 79
	    ldi r11 79
	    ldi r12 94
	    ldi r13 23
	    ldi r14 79
	    ldi r15 212
	.fn_load_batch__skip13
	adi r1 1
	brh nc .fn_load_batch__skip14
	    ldi r2 20
	    ldi r3 174
	    ldi r4 110
	    ldi r5 8
	    ldi r6 30
	    ldi r7 54
	    ldi r8 14
	    ldi r9 161
	    ldi r10 20
	    ldi r11 174
	    ldi r12 83
	    ldi r13 143
	    ldi r14 235
	    ldi r15 158
	.fn_load_batch__skip14
	adi r1 1
	brh nc .fn_load_batch__skip15
	    ldi r2 0
	    ldi r3 176
	    ldi r4 138
	    ldi r5 83
	    ldi r6 74
	    ldi r7 0
	    ldi r8 138
	    ldi r9 216
	    ldi r10 248
	    ldi r11 42
	    ldi r12 74
	    ldi r13 30
	    ldi r14 41
	    ldi r15 182
	.fn_load_batch__skip15
	adi r1 1
	brh nc .fn_load_batch__skip16
	    ldi r2 79
	    ldi r3 79
	    ldi r4 79
	    ldi r5 79
	    ldi r6 79
	    ldi r7 247
	    ldi r8 193
	    ldi r9 99
	    ldi r10 143
	    ldi r11 190
	    ldi r12 158
	    ldi r13 128
	    ldi r14 99
	    ldi r15 172
	.fn_load_batch__skip16
	adi r1 1
	brh nc .fn_load_batch__skip17
	    ldi r2 247
	    ldi r3 79
	    ldi r4 133
	    ldi r5 79
	    ldi r6 79
	    ldi r7 151
	    ldi r8 79
	    ldi r9 143
	    ldi r10 79
	    ldi r11 235
	    ldi r12 79
	    ldi r13 143
	    ldi r14 79
	    ldi r15 79
	.fn_load_batch__skip17
	adi r1 1
	brh nc .fn_load_batch__skip18
	    ldi r2 119
	    ldi r3 79
	    ldi r4 79
	    ldi r5 136
	    ldi r6 42
	    ldi r7 54
	    ldi r8 143
	    ldi r9 161
	    ldi r10 143
	    ldi r11 133
	    ldi r12 62
	    ldi r13 103
	    ldi r14 79
	    ldi r15 190
	.fn_load_batch__skip18
	adi r1 1
	brh nc .fn_load_batch__skip19
	    ldi r2 128
	    ldi r3 0
	    ldi r4 0
	    ldi r5 186
	    ldi r6 80
	    ldi r7 90
	    ldi r8 14
	    ldi r9 158
	    ldi r10 158
	    ldi r11 110
	    ldi r12 167
	    ldi r13 94
	    ldi r14 129
	    ldi r15 158
	.fn_load_batch__skip19
	adi r1 1
	brh nc .fn_load_batch__skip20
	    ldi r2 128
	    ldi r3 0
	    ldi r4 57
	    ldi r5 97
	    ldi r6 176
	    ldi r7 80
	    ldi r8 9
	    ldi r9 94
	    ldi r10 74
	    ldi r11 0
	    ldi r12 186
	    ldi r13 216
	    ldi r14 216
	    ldi r15 138
	.fn_load_batch__skip20
	adi r1 1
	brh nc .fn_load_batch__skip21
	    ldi r2 143
	    ldi r3 79
	    ldi r4 9
	    ldi r5 20
	    ldi r6 135
	    ldi r7 97
	    ldi r8 170
	    ldi r9 110
	    ldi r10 110
	    ldi r11 133
	    ldi r12 164
	    ldi r13 90
	    ldi r14 158
	    ldi r15 23
	.fn_load_batch__skip21
	adi r1 1
	brh nc .fn_load_batch__skip22
	    ldi r2 79
	    ldi r3 158
	    ldi r4 79
	    ldi r5 131
	    ldi r6 79
	    ldi r7 103
	    ldi r8 151
	    ldi r9 250
	    ldi r10 79
	    ldi r11 83
	    ldi r12 79
	    ldi r13 10
	    ldi r14 143
	    ldi r15 20
	.fn_load_batch__skip22
	adi r1 1
	brh nc .fn_load_batch__skip23
	    ldi r2 25
	    ldi r3 158
	    ldi r4 203
	    ldi r5 79
	    ldi r6 133
	    ldi r7 158
	    ldi r8 247
	    ldi r9 97
	    ldi r10 103
	    ldi r11 167
	    ldi r12 79
	    ldi r13 190
	    ldi r14 143
	    ldi r15 199
	.fn_load_batch__skip23
	adi r1 1
	brh nc .fn_load_batch__skip24
	    ldi r2 248
	    ldi r3 0
	    ldi r4 32
	    ldi r5 0
	    ldi r6 0
	    ldi r7 0
	    ldi r8 216
	    ldi r9 48
	    ldi r10 0
	    ldi r11 158
	    ldi r12 105
	    ldi r13 158
	    ldi r14 174
	    ldi r15 65
	.fn_load_batch__skip24
	adi r1 1
	brh nc .fn_load_batch__skip25
	    ldi r2 169
	    ldi r3 79
	    ldi r4 133
	    ldi r5 115
	    ldi r6 79
	    ldi r7 94
	    ldi r8 158
	    ldi r9 5
	    ldi r10 161
	    ldi r11 151
	    ldi r12 185
	    ldi r13 41
	    ldi r14 235
	    ldi r15 128
	.fn_load_batch__skip25
	adi r1 1
	brh nc .fn_load_batch__skip26
	    ldi r2 158
	    ldi r3 79
	    ldi r4 250
	    ldi r5 158
	    ldi r6 14
	    ldi r7 79
	    ldi r8 94
	    ldi r9 83
	    ldi r10 110
	    ldi r11 79
	    ldi r12 158
	    ldi r13 167
	    ldi r14 119
	    ldi r15 97
	.fn_load_batch__skip26
	adi r1 1
	brh nc .fn_load_batch__skip27
	    ldi r2 50
	    ldi r3 0
	    ldi r4 99
	    ldi r5 70
	    ldi r6 218
	    ldi r7 246
	    ldi r8 30
	    ldi r9 133
	    ldi r10 74
	    ldi r11 137
	    ldi r12 158
	    ldi r13 79
	    ldi r14 112
	    ldi r15 147
	.fn_load_batch__skip27
	adi r1 1
	brh nc .fn_load_batch__skip28
	    ldi r2 158
	    ldi r3 158
	    ldi r4 20
	    ldi r5 42
	    ldi r6 14
	    ldi r7 185
	    ldi r8 128
	    ldi r9 0
	    ldi r10 62
	    ldi r11 83
	    ldi r12 16
	    ldi r13 0
	    ldi r14 87
	    ldi r15 158
	.fn_load_batch__skip28
	adi r1 1
	brh nc .fn_load_batch__skip29
	    ldi r2 30
	    ldi r3 158
	    ldi r4 14
	    ldi r5 41
	    ldi r6 158
	    ldi r7 158
	    ldi r8 158
	    ldi r9 158
	    ldi r10 62
	    ldi r11 158
	    ldi r12 158
	    ldi r13 158
	    ldi r14 94
	    ldi r15 164
	.fn_load_batch__skip29
	adi r1 1
	brh nc .fn_load_batch__skip30
	    ldi r2 79
	    ldi r3 79
	    ldi r4 151
	    ldi r5 235
	    ldi r6 97
	    ldi r7 107
	    ldi r8 133
	    ldi r9 122
	    ldi r10 83
	    ldi r11 133
	    ldi r12 94
	    ldi r13 158
	    ldi r14 158
	    ldi r15 158
	.fn_load_batch__skip30



	// Put batch in RAM
	ldi r1 FIRST_LAYER_BYTE_LOAD_ADDR
	adi r1 8
	str r1 r2 -8
	str r1 r3 -7
	str r1 r4 -6
	str r1 r5 -5
	str r1 r6 -4
	str r1 r7 -3
	str r1 r8 -2
	str r1 r9 -1
	str r1 r10 0
	str r1 r11 1
	str r1 r12 2
	str r1 r13 3
	str r1 r14 4
	str r1 r15 5

ret