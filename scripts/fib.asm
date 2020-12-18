	addi $t9, $zero, 268500992
	addi $t0, $zero, 0
	sw $t0, 0($t9)
	addi $t0, $zero, 1
	sw $t0, 4($t9)
	addi $t0, $zero, 2
	sw $t0, 80($t9)
loop0:
	lw $t0, 80($t9)
	addiu $t0, $t0, -1
	addu $t1, $zero, $t0
	sw $t1, 84($t9)
	lw $t0, 80($t9)
	addiu $t0, $t0, -2
	addu $t1, $zero, $t0
	sw $t1, 88($t9)
	lw $t0, 88($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 0($t0)
	addu $t1, $zero, $t0
	lw $t0, 84($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 0($t0)
	addu $t0, $t0, $t1
	addu $t1, $zero, $t0
	lw $t0, 80($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t1, 0($t0)
	lw $t0, 80($t9)
	addi $t0, $t0, 1
	sw $t0, 80($t9)
	lw $t0, 80($t9)
	slti $t0, $t0, 20
	xori $t0, $t0, 1
	blez $t0 loop0
