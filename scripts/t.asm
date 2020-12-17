addi $t0, $zero, 10
addi $t1, $zero, 3		# one
addi $t2, $zero, 1		# one
slt $t0, $t1, $t2
blez $t0, done
nop
addi $t9, $zero, 268500992 	# array base address
addi $t1, $zero, 3		# one
sw $t1, 0($t9)			# a[0] = 1
done:
lw $t0, 0($t9)			# load old
addi $t0, $t0, 1		# inc
sw $t0, 0($t9)			# a[0] = 1
nop