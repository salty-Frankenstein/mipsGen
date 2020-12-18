	addi $t9, $zero, 268697600
	addi $t0, $zero, 1
	sw $t0, 0($t9)
	addi $t0, $zero, 2
	sw $t0, 4($t9)
	lw $t0, 4($t9)
	addu $t4, $zero, $t0
	lw $t0, 0($t9)
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t1, $zero, $t0
	sw $t1, 8($t9)
	addi $t0, $zero, -1
	sw $t0, 4($t9)
	lw $t0, 4($t9)
	addu $t4, $zero, $t0
	lw $t0, 0($t9)
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t1, $zero, $t0
	sw $t1, 8($t9)
	addi $t0, $zero, 1
	sw $t0, 4($t9)
	lw $t0, 4($t9)
	addu $t4, $zero, $t0
	lw $t0, 0($t9)
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t1, $zero, $t0
	sw $t1, 8($t9)
