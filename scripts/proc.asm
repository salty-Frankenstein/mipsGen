	addi $fp, $zero, 268697596
	addi $sp, $zero, 268697596
	j main
proc0:
	addi $sp, $sp, 4
	sw $ra, ($sp)
	addi $sp, $sp, 4
	sw $fp, ($sp)
	addu $fp, $zero, $sp
	lw $t0, -8($fp)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label1
	nop
	addi $t0, $zero, 1
	addu $v0, $zero, $t0
	addu $sp, $zero, $fp
	lw $fp, ($sp)
	addi $sp, $sp, -4
	lw $ra, ($sp)
	addi $sp, $sp, -4
	jr $ra
	j done_label1
	nop
false_label1:
	addi $sp, $sp, 4
	lw $t0, -8($fp)
	addiu $t0, $t0, -1
	addi $sp, $sp, 4
	sw $t0, ($sp)
	jal proc0
	addu $t0, $zero, $v0
	addu $t5, $zero, $t0
	sw $t5, 4($fp)
	lw $t0, -8($fp)
	addu $t6, $zero, $t0
	lw $t0, 4($fp)
	addu $t7, $zero, $t0
	mul $t0, $t6, $t7
	addu $v0, $zero, $t0
	addu $sp, $zero, $fp
	lw $fp, ($sp)
	addi $sp, $sp, -4
	lw $ra, ($sp)
	addi $sp, $sp, -4
	jr $ra
done_label1:
proc2:
	addi $sp, $sp, 4
	sw $ra, ($sp)
	addi $sp, $sp, 4
	sw $fp, ($sp)
	addu $fp, $zero, $sp
	lw $t0, -8($fp)
	addu $v0, $zero, $t0
	addu $sp, $zero, $fp
	lw $fp, ($sp)
	addi $sp, $sp, -4
	lw $ra, ($sp)
	addi $sp, $sp, -4
	jr $ra
main:
	addi $sp, $sp, 4
	addi $t0, $zero, 10
	addi $sp, $sp, 4
	sw $t0, ($sp)
	jal proc0
	addu $t0, $zero, $v0
	addu $t5, $zero, $t0
	sw $t5, 4($fp)
	addi $sp, $sp, 4
	addi $t5, $zero, 1
	sw $t5, 8($fp)
	addi $sp, $sp, 4
	addi $t5, $zero, 2
	sw $t5, 12($fp)
	lw $t0, 12($fp)
	addi $sp, $sp, 4
	sw $t0, ($sp)
	lw $t0, 8($fp)
	addi $sp, $sp, 4
	sw $t0, ($sp)
	jal proc2
	addu $t0, $zero, $v0
	addu $t5, $zero, $t0
	sw $t5, 4($fp)
