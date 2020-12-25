	addi $fp, $zero, 268697596
	addi $sp, $zero, 268697596
	j main
main:
	addi $sp, $sp, 4
	addi $sp, $sp, 4
	addi $t5, $zero, 1
	sw $t5, 4($fp)
	addi $t5, $zero, 2
	sw $t5, 8($fp)
	addi $sp, $sp, 4
	lw $t0, 8($fp)
	addu $t4, $zero, $t0
	lw $t0, 4($fp)
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t5, $zero, $t0
	sw $t5, 12($fp)
	addi $t5, $zero, -1
	sw $t5, 8($fp)
	lw $t0, 8($fp)
	addu $t4, $zero, $t0
	lw $t0, 4($fp)
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t5, $zero, $t0
	sw $t5, 12($fp)
	addi $t5, $zero, 1
	sw $t5, 8($fp)
	lw $t0, 8($fp)
	addu $t4, $zero, $t0
	lw $t0, 4($fp)
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t5, $zero, $t0
	sw $t5, 12($fp)
