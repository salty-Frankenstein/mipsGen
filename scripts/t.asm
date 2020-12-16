addi $t9, $zero, 268500992 	# array base address
addi $t1, $zero, 3		# one
sw $t1, 0($t9)			# a[0] = 1

lw $t0, 0($t9)			# load old
addi $t0, $t0, 1		# inc
sw $t0, 0($t9)			# a[0] = 1