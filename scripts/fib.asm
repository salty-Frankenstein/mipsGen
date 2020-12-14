# fib.asm: Compute first 40 Fibonacci numbers and put in array
	#addi $t9, $zero, 0x10010000 	# array base address
	addi $t9, $zero, 0x0 		# array base address
	addi $t1, $zero, 1		# one
	sw $t1, 0($t9)			# a[0] = 1
	sw $t1, 4($t9)			# a[1] = 1
	
	# loop variables
	addi $t0, $zero, 3		# for i = 3
	addi $t1, $zero, 40		# to 40
	
	addi $t3, $t9, 8		# pointer
loop:	
	# loop body
	lw $t4, -8($t3)			# get a[i-2]
	lw $t5, -4($t3)			# get a[i-1]
	
	add $t4, $t4, $t5		# a[i-1] + a[i-2]
	sw $t4, 0($t3)			# a[i] = a[i-1] + a[i-2]
	
	addi $t3, $t3, 4		# next elem
	
	addi $t0, $t0, 1		#inc i
	subu $t2, $t0, $t1
	blez $t2, loop
