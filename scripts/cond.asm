	addi $fp, $zero, 268697596
	addi $sp, $zero, 268697596
	j main
main:
	addi $sp, $sp, 4
	addi $sp, $sp, 4
	addi $t5, $zero, 10
	sw $t5, 4($fp)
	lw $t0, 4($fp)
	slti $t0, $t0, 5
	blez $t0 false_label0
	nop
	addi $t5, $zero, 1
	sw $t5, 8($fp)
	j done_label0
	nop
false_label0:
	addi $t5, $zero, 2
	sw $t5, 8($fp)
done_label0:
	addi $sp, $sp, 4
	lw $t0, 4($fp)
	addi $t4, $zero, 10
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t6, $zero, $t0
	lw $t0, 8($fp)
	addi $t4, $zero, 1
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t7, $zero, $t0
	xori $t1, $t6, 1
	xori $t2, $t7, 1
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t5, $zero, $t0
	sw $t5, 12($fp)
	addi $sp, $sp, 4
	lw $t0, 4($fp)
	addi $t4, $zero, 10
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t6, $zero, $t0
	lw $t0, 8($fp)
	addi $t4, $zero, 1
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t7, $zero, $t0
	xori $t1, $t6, 1
	xori $t2, $t7, 1
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label1
	nop
	addi $t5, $zero, 84
	sw $t5, 16($fp)
	j done_label1
	nop
false_label1:
	addi $t5, $zero, 70
	sw $t5, 16($fp)
done_label1:
	addi $sp, $sp, 4
	lw $t0, 8($fp)
	addi $t4, $zero, 1
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t6, $zero, $t0
	lw $t0, 4($fp)
	addi $t4, $zero, 10
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t7, $zero, $t0
	xori $t1, $t6, 1
	xori $t2, $t7, 1
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label2
	nop
	addi $t5, $zero, 84
	sw $t5, 20($fp)
	j done_label2
	nop
false_label2:
	addi $t5, $zero, 70
	sw $t5, 20($fp)
done_label2:
