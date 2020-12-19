	addi $t9, $zero, 268697600
	addi $t0, $zero, 10
	sw $t0, 0($t9)
	lw $t0, 0($t9)
	slti $t0, $t0, 5
	blez $t0 false_label0
	addi $t0, $zero, 1
	sw $t0, 4($t9)
	j done_label0
	nop
false_label0:
	addi $t0, $zero, 2
	sw $t0, 4($t9)
done_label0:
