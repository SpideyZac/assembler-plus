// by ProtoSebastian
// bubble sort, heap sort, and
// quick sort (Hoare/Lomuto partitioning)
// in BatPU-v2 assembly with dynamic arrays.
// [length of elements, elements...]
//  ^~~~~~~~~~~~~~~~~~
//   base points here

define ARRAY_BASE 11  // where the array starts
define STACK_BASE 128 // where the stack starts
define PORTS_BASE 248
// TEMP: 0 to 7
// HEAP: 8 - 127
// STACK: 128 onwards

  ldi r15 PORTS_BASE
  ldi r14 ARRAY_BASE
  ldi r10 STACK_BASE // stack pointer

// does bubble sort -> heap sort -> quick sort, each with various patterns
// bubble sort
  cal .wait_until_input
  cal .decreasing_init
  cal .bubble_sort
  cal .done_title

  cal .wait_until_input
  cal .pipe_organ_init
  cal .bubble_sort
  cal .done_title

  cal .wait_until_input
  cal .inverse_pipe_organ_init
  cal .bubble_sort
  cal .done_title

  cal .wait_until_input
  cal .bubble_sort
  cal .done_title

  cal .wait_until_input
  cal .shuffle
  cal .bubble_sort
  cal .done_title

// heap sort
  cal .wait_until_input
  cal .decreasing_init
  cal .heap_sort_init
  cal .done_title

  cal .wait_until_input
  cal .pipe_organ_init
  cal .heap_sort_init
  cal .done_title

  cal .wait_until_input
  cal .inverse_pipe_organ_init
  cal .heap_sort_init
  cal .done_title

  cal .wait_until_input
  cal .heap_sort_init
  cal .done_title

  cal .wait_until_input
  cal .shuffle
  cal .heap_sort_init
  cal .done_title

// quick sort (Hoare by default)
  cal .wait_until_input
  cal .decreasing_init
  cal .qsort_init
  cal .done_title

  cal .wait_until_input
  cal .pipe_organ_init
  cal .qsort_init
  cal .done_title

  cal .wait_until_input
  cal .inverse_pipe_organ_init
  cal .qsort_init
  cal .done_title

  cal .wait_until_input
  cal .qsort_init
  cal .done_title

  cal .wait_until_input
  cal .shuffle
  cal .qsort_init
  cal .done_title

  hlt

.done_title
  str r15 r0 1
  ldi r1 'D'
  str r15 r1 -1
  ldi r1 'O'
  str r15 r1 -1
  ldi r1 'N'
  str r15 r1 -1
  ldi r1 'E'
  str r15 r1 -1
  str r15 r0 0
  ret

.init_title
  str r15 r0 1
  ldi r1 'I'
  str r15 r1 -1
  ldi r2 'N'
  str r15 r2 -1
  str r15 r1 -1
  ldi r2 'T'
  str r15 r2 -1
  str r15 r0 0
  ret

.increasing_init
  cal .init_title
  cal .clear_array
  ldi r1 1
  ldi r4 32
.increasing_array_loop
  cal .append
  adi r1 1
  dec r4
  brh nz .increasing_array_loop
  ret

.decreasing_init
  cal .init_title
  cal .clear_array
  ldi r1 32
  ldi r4 32
.decreasing_array_loop
  cal .append
  adi r1 -1
  dec r4
  brh nz .decreasing_array_loop
  ret

.pipe_organ_init
  cal .init_title
  cal .clear_array
  ldi r1 1
  ldi r4 16
.pipe_organ_part_1
  cal .append
  adi r1 2
  dec r4
  brh nz .pipe_organ_part_1

  ldi r1 32
  ldi r4 16
.pipe_organ_part_2
  cal .append
  adi r1 -2
  dec r4
  brh nz .pipe_organ_part_2
  ret

.inverse_pipe_organ_init
  cal .init_title
  cal .clear_array
  ldi r1 32
  ldi r4 16
.inverse_pipe_organ_part_1
  cal .append
  adi r1 -2
  dec r4
  brh nz .inverse_pipe_organ_part_1

  ldi r1 1
  ldi r4 16
.inverse_pipe_organ_part_2
  cal .append
  adi r1 2
  dec r4
  brh nz .inverse_pipe_organ_part_2
  ret

.clear_array
  str r15 r0 -2
  lod r14 r1
  str r14 r0
.clear_array_loop
  add r14 r1 r2
  str r2 r0 1
  dec r1
  brh c  .clear_array_loop
  ret

.shuffle_init
  cal .increasing_init
  cal .shuffle
  ret

/// wait until input
.wait_until_input
  lod r15 r1 7
  cmp r1 r0
  brh eq .wait_until_input
  ret
///

/// shuffle procedure
// 
.shuffle

  str r15 r0 1
  ldi r2 'S'
  str r15 r2 -1
  ldi r2 'H'
  str r15 r2 -1
  ldi r2 'U'
  str r15 r2 -1
  ldi r2 'F'
  str r15 r2 -1
  str r15 r2 -1
  ldi r2 'L'
  str r15 r2 -1
  ldi r2 'E'
  str r15 r2 -1
  str r15 r0 0

  ldi r5 1
  ldi r6 0x1F
.shuffle_loop
  lod r15 r2 6 // take random number
  cmp r5 r2 // make it look more "random"
  brh lt .shuffle_loop // reroll if r2>r1 (r1<r2)
  mov r5 r1 // load i
  and r2 r6 r2
  cal .swap
  inc r5
  cmp r6 r5
  brh ge .shuffle_loop
  ret
///

/// max heapify array
// input: heap length = r9,  array base = r14
// extra: r5 (current node)
.max_heapify
  rsh r9 r5 // get half of size..
  adi r5 -1 // -1 to get index of the last node with atleast 1 child
.max_heapify_loop // start of max heapify loop
  cal .sift_down  // sift down curret node
  dec r5          // move onto next node
  brh c  .max_heapify_loop // keep looping until r5 overflows

  ret // return from sub-routine
///

/// heap sort
// input: base of dynamic array = r14
// extra: r5 (current node), r9 (heap length), r1, r2 (for swapping) (unsafe)
.heap_sort_init

  str r15 r0 1
  ldi r1 'H'
  str r15 r1 -1
  ldi r1 'E'
  str r15 r1 -1
  ldi r1 'A'
  str r15 r1 -1
  ldi r1 'P'
  str r15 r1 -1
  str r15 r0 0

  lod r14 r9       // load dynamic array size
  cal .max_heapify // max heapify array
  xor r5 r5 r5     // reset r5 (current node = 0)
.heap_sort_loop    // heap sort loop start
  mov r5 r1        // move current node to r1 to get ready for swapping
  dec r9           // decrement size to decommission latest sorted element, and get the index of the top of the array
  brh z  .heap_sort_exit // if zero, exit (sorting finished!)
  mov r9 r2        // move r9 (top of array) to r2 to get ready for swapping
  cal .swap        // swap
  cal .sift_down   // sift down node 0
  jmp .heap_sort_loop // loop
.heap_sort_exit
  ret // return from sub-routine
///

/// sift down procedure
// input: node = r5, len = r9
// extra: r6 (current node), r7 (left child/bigger child), r8 (right child/temp current node) (unsafe)
.sift_down
  mov r5 r6      // copy r5 (starting node) to r6 (current node)
.sift_down_loop
  lsh r6 r7      // get left child by doing (2*current_node)..
  inc r7         // + 1
  cmp r7 r9      // check if left child is within bounds
  brh ge .sift_down_exit // exit if not (sift down finished!)
  add r14 r7 r12 // get pointer to left child
  lod r12 r3 1   // load left child's value
  mov r7 r8      // get right child by getting left child's index and..
  inc r8         // and adding 1 ((2*node) + 2)
  cmp r8 r9      // check if right child is within bounds
  brh ge .sift_down_1_child // skip if not (node has only 1 child)
  // node has 2 children
  add r14 r8 r13 // get pointer to right child
  lod r13 r4 1   // load right child's value
  cmp r3 r4      // compare left and right child's values
  brh ge .sift_down_child1_bigger // if left child bigger, continue
  mov r8 r7      // if right child is bigger, overwrite left child's index with right child's index
  mov r13 r12    // and the pointer
  mov r4 r3      // and its value
.sift_down_child1_bigger
.sift_down_1_child
  // bigger child value already loaded in r3
  // bigger child pointer already loaded in r12
  add r14 r6 r13 // get pointer to node
  lod r13 r4 1   // load node's value
  cmp r4 r3      // compare bigger child's and node's values
  brh ge .sift_down_no_swap // if node's value is bigger, skip swap
  // swap bigger child and node
  mov r7 r1   // get ready for swap..
  mov r6 r2   //
  cal .swap_2 // swap
.sift_down_no_swap
  mov r7 r6   // switch to child node..
  jmp .sift_down_loop // and continue
.sift_down_exit
  ret // return from sub-routine
///

/// initialize qsort
// input: top of stack = r10, start of array = r14
// extra: stack, r1 (start), r2 (end), r3 (middle), r11 (stack base) (unsafe)
.qsort_init

  str r15 r0 1
  ldi r1 'Q'
  str r15 r1 -1
  ldi r1 'U'
  str r15 r1 -1
  ldi r1 'I'
  str r15 r1 -1
  ldi r1 'C'
  str r15 r1 -1
  ldi r1 'K'
  str r15 r1 -1
  str r15 r0 0

  xor r1 r1 r1 // reset r1
  lod r14 r2   // set r2 to n - 1
  dec r2
  // push to stack
  str r10 r1 0 // push start index
  str r10 r2 1 // push end index
  adi r10 2    // adjust top of stack
  ldi r11 STACK_BASE
  // "iterative" quick-sort, artificially brings up max recursion level to (248 - STACK_BASE)
.iterative_qsort_loop
  cal .qsort  // call quick sort
  cmp r10 r11 // compare top of stack and base of stack
  brh ne .iterative_qsort_loop // if not equal, there's still work to be done.

  ret // return from sub-routine
// input: end = stack first, start = stack second
// extra: r1 (start), r2 (end), r3 (middle) (unsafe)
// 3 lines have to be switched for switching Lomuto/Hoare partitioning
.qsort
  // grab parameters
  lod r10 r2 -1 // pop end index
  lod r10 r1 -2 // pop start index
  adi r10 -2    // adjust top of stack
  cmp r1 r2     // leave if start >= end
  brh ge .qsort_return
  // special signed case
  nor r2 r0 r0  // end might be 255 (-1)
  brh z  .qsort_return // leave
  str r0 r1 2  // store r1 and r2 in temp
  str r0 r2 3  // ..

  add r1 r2 r3 // middle (rounded to closest integer)
  inc r3       // ..
  rsh r3 r3    // ..

// take a sample from start, end, and middle and use the median as the pivot
  add r14 r1 r4 // start
  add r14 r2 r5 // end
  add r14 r3 r6 // middle
  lod r4 r1 1   // start element
  lod r5 r2 1   // end element
  lod r6 r3 1   // middle element

  // smart logic for median
  cmp r3 r2
  brh lt .median_cond0
  xor r5 r6 r5
  xor r5 r6 r6
  xor r5 r6 r5
  lod r5 r2 1
  lod r6 r3 1
.median_cond0
  cmp r1 r2
  brh ge .median_cond1
  cmp r3 r1
  brh lt .median_cond2
  mov r6 r4
  jmp .median_cond2
.median_cond1
  mov r5 r4
.median_cond2
  // median pointer is now in r5
  sub r4 r14 r2 // get median index from pointer
  lod r0 r1 2   // load low index from temp storage ; comment for Lomuto, uncomment for Hoare
//  lod r0 r1 3   // load high index from temp storage ; uncomment for Lomuto, comment for Hoare

  cal .swap    // swap median guess element with start for pivot
  lod r0 r2 3  // load back r1 and r2 from temp for partitioning
  lod r0 r1 2  // ..
  cal .Hoare_partition // partition subarray ; comment for Lomuto, uncomment for Hoare
//  cal .Lomuto_partition // partition subarray ; uncomment for Lomuto, comment for Hoare
  lod r0 r2 3  // load r1 and r2 again after partition step
  lod r0 r1 2  // ..
  // r3 should be partition index
  str r10 r1 2 // start = start
  str r10 r3 3 // end = pi
  adi r3  1    // start = pi + 1 ; comment for Lomuto, uncomment for Hoare
//  adi r3  2    // start = pi + 1 ; uncomment for Lomuto, comment for Hoare
  str r10 r3 0 // ..
  str r10 r2 1 // end = end
  adi r10 4    // adjust top of stack
.qsort_return
  ret // return from sub-routine
///

/// Hoare partition, 1 register less than Lomuto, and faster on average
// input: start = r1, end = r2 (unsafe), start of array = r14 (safe)
// extra: r3 (value of array[A]), r4 (value of array[B]), r5 (index A), r6 (index B), r7 (pivot value), r12 (pointer to array[i]), r13 (pointer to array[j])
// output: partition index = r3
.Hoare_partition
  add r14 r1 r13 // pointer to array[start]
  lod r13 r7 r1  // load pivot value; array[start]
  mov r1 r5      // A = start..
  dec r5         // - 1
  mov r2 r6      // B = end..
  inc r6         // + 1

  .Hoare_loop    // Hoare while loop
  .Hoare_A_loop  // Hoare A loop
  inc r5         // increment A
  add r14 r5 r12 // get pointer to array[A]
  lod r12 r3 1   // load array[A]
  cmp r3 r7      // compare array[A] with pivot
  brh lt .Hoare_A_loop // keep going if lesser than

  .Hoare_B_loop  // Hoare B loop
  dec r6         // decrement B
  add r14 r6 r13 // get pointer to array[B]
  lod r13 r4 1   // load array[B]
  cmp r7 r4      // compare pivot with array[B]
  brh lt .Hoare_B_loop // keep going if lesser than

  cmp r5 r6
  brh ge .Hoare_exit

  mov r5 r1
  mov r6 r2
  cal .swap_2

  jmp .Hoare_loop
.Hoare_exit
  mov r6 r3
  ret
///

/// Lomuto partition, 1 register more than Hoare, and slower on average
// input: start = r1, end = r2 (unsafe), start of array = r14 (safe)
// extra: r3 (value of array[A]), r4 (value of array[B]), r5 (index A), r6 (index B), r7 (pivot value), r8 (for loop bound), r12 (pointer to array[i]), r13 (pointer to array[j])
// output: partition index = r3
.Lomuto_partition
  add r14 r2 r13 // pointer to array[end]
  lod r13 r7 1   // load pivot value; array[end]
  mov r1 r5      // A = start..
  dec r5         // - 1
  mov r1 r6      // B = start

  add r14 r5 r12 // pointer to array[A]
  add r14 r6 r13 // pointer to array[B]

  mov r2 r8 // limit = high..
  dec r8    // - 1
.Lomuto_for_loop  // start of Lomuto partition loop
  lod r13 r4 1   // load value of array[B]
  cmp r7 r4      // compare with pivot
  brh lt .Lomuto_no_swap // don't swap if lesser than pivot
  // swap array[A] and array[B]
  inc r5         // increment A..
  inc r12        // and its pointer
  lod r12 r3 1   // load value at new A index
  str r0 r1 0    // store r1 and r2 in temp
  str r0 r2 1    // ..
  mov r5 r1      // get ready for swap
  mov r6 r2      // ..
  cal .swap_2    // swap
  lod r0 r2 1    // load back r1 and r2 from temp
  lod r0 r1 0    // ..
.Lomuto_no_swap
  inc r6         // increment B
  add r14 r5 r12 // pointer to array[A]
  add r14 r6 r13 // pointer to array[B]
  cmp r8 r6      // compare B and limit
  brh ge .Lomuto_for_loop

  inc r12
  add r14 r2 r13
  mov r5 r1
  inc r1
  lod r12 r3 1
  lod r13 r4 1
  str r0 r1 0
  str r0 r2 1
  cal .swap_2
  lod r0 r2 1
  lod r0 r1 0
  
  mov r5 r3
  ret
///

.bubble_sort

  str r15 r0 1
  ldi r2 'B'
  str r15 r2 -1
  ldi r1 'U'
  str r15 r1 -1
  str r15 r2 -1
  str r15 r2 -1
  ldi r1 'L'
  str r15 r1 -1
  ldi r1 'E'
  str r15 r1 -1
  str r15 r0 0

  lod r14 r5 // size
  dec r5 // - 1
  ldi r7 0 // i
.outer_loop
  // initialize
  mov r5 r6 // size - 1
  sub r6 r7 r6 // - i
  ldi r1 0 // j
  ldi r9 0 // swapped = False
.inner_loop
  mov r1 r2
  inc r2
  add r14 r1 r12
  add r14 r2 r13
  lod r12 r3 1
  lod r13 r4 1
  cmp r4 r3
  brh ge .skip_swap
  nor r0 r0 r9
  str r0 r1 0
  str r0 r2 1
  cal .swap_2
  lod r0 r2 1
  lod r0 r1 0
.skip_swap
  inc r1
  dec r6
  brh nz .inner_loop
  cmp r9 r0
  brh ne .swapped
  ret // sorted! early exit
.swapped
  inc r7
  cmp r7 r5
  brh ne .outer_loop

  ret // sorted!

// input: Value = r1, Array rbp = r14
// extra: r2, r3
.append
  lod r14 r2
  add r14 r2 r3
  str r3  r1 1
  inc r2
  str r14 r2
  dec r2
  mov r1 r3
  cal .vline1
  ret

// input: X = r3, Height = r2, Ports rbp = r15
// extra: r2
.vline1
  str r15 r2 -8
  dec r3
.loop_vline1
  str r15 r3 -7
  str r15 r0 -6
//  str r15 r0 -3 // aggressive buffering
  dec r3
  brh c  .loop_vline1

  str r15 r0 -3
  ret

// input: A = r1, B = r2, Array rbp = r14, Ports rbp = r15
// extra: r3, r4, r12, r13 (unsafe)
.swap
  add r14 r1 r12
  add r14 r2 r13
  lod r12 r3 1
  lod r13 r4 1
.swap_2
  str r12 r4 1
  str r13 r3 1
// displaying part
  sub r4 r3 r4
  brh z  .bneg_end
  brh lt .bneg
  sub r0 r4 r4
  mov r12 r13
  lod r13 r3 1
  xor r1 r2 r1
  xor r1 r2 r2
  xor r1 r2 r1
.bneg

  str r15 r1 -8
.aneg_loop0
  dec r3
  str r15 r3 -7
  str r15 r0 -5
  //str r15 r0 -3 // aggressive buffer
  inc r4
  brh nz .aneg_loop0

  lod r13 r4 1
  sub r4 r3 r4
  str r15 r2 -8
.aneg_loop1
  str r15 r3 -7
  str r15 r0 -6
  //str r15 r0 -3 // aggressive buffer
  inc r3
  dec r4
  brh nz .aneg_loop1
.bneg_end
  str r15 r0 -3

  ret