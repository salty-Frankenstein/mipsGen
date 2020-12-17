	addi $t9, $zero, 268500992
	addi $t0, $zero, 1
	sw $t0, 4($t9)
loop0:
	lw $t0, 0($t9)
	addi $t0, $t0, 1
	sw $t0, 0($t9)
	lw $t0, 4($t9)
	addi $t0, $t0, 1
	sw $t0, 4($t9)
	lw $t0, 4($t9)
	slti $t0, $t0, 10
	xori $t0, $t0, 1
	blez $t0 loop0
