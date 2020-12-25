j main
f1:
addu $t0, $t1, $t2
jr $ra


main:
addi $t1, $zero, 1
addi $t2, $zero, 2
jal f1


addi $t0, $zero, 5
addi $t1, $zero, 3
mul $t0, $t1, $t0

addi $t0, $zero, 1
addi $t1, $zero, 1
addi $t2, $zero, 1
addi $t3, $zero, 1
nor $t0, $t0, $t0


sll $t0, $t0, 2

addi $t1, $zero, 3
mul $t0, $t1, $t0
div $t0, $t1, $t0
#div $t0, $t1
mflo $t0


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

