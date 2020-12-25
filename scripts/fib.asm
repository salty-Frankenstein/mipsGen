	addi $fp, $zero, 268697600
	addi $sp, $zero, 268697600
	addi $t5, $zero, 0
	sw $t5, 0($fp)
	addi $t5, $zero, 1
	sw $t5, 4($fp)
	addi $t5, $zero, 2
	sw $t5, 80($fp)
loop0:
	lw $t0, 80($fp)
	addiu $t0, $t0, -1
	addu $t5, $zero, $t0
	sw $t5, 84($fp)
	lw $t0, 80($fp)
	addiu $t0, $t0, -2
	addu $t5, $zero, $t0
	sw $t5, 88($fp)
	lw $t0, 88($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 0($t0)
	addu $t1, $zero, $t0
	lw $t0, 84($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 0($t0)
	addu $t0, $t0, $t1
	addu $t5, $zero, $t0
	lw $t0, 80($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 0($t0)
	lw $t0, 80($fp)
	addi $t0, $t0, 1
	sw $t0, 80($fp)
	lw $t0, 80($fp)
	slti $t0, $t0, 20
	xori $t0, $t0, 1
	blez $t0 loop0
	nop
