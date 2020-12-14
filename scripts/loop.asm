	addi $t0, $zero, 1		# for 1
	addi $t1, $zero, 5		# to 5
	addi $t3, $zero, 1		# a dder
loop:	
	add $t3, $t3, $t3		# loop body
	
	addi $t0, $t0, 1		#inc
	subu $t2, $t0, $t1
	blez $t2, loop