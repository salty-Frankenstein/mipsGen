	addi $t9, $zero, 268697600
	addi $t5, $zero, 10
	sw $t5, 0($t9)
	lw $t0, 0($t9)
	slti $t0, $t0, 5
	blez $t0 false_label0
	addi $t5, $zero, 1
	sw $t5, 4($t9)
	j done_label0
	nop
false_label0:
	addi $t5, $zero, 2
	sw $t5, 4($t9)
done_label0:
	nop
	nop
	nop
	nop
	lw $t0, 0($t9)
	addi $t4, $zero, 10
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t6, $zero, $t0
	lw $t0, 4($t9)
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
	sw $t5, 8($t9)
	lw $t0, 0($t9)
	addi $t4, $zero, 10
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t6, $zero, $t0
	lw $t0, 4($t9)
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
	addi $t5, $zero, 84
	sw $t5, 12($t9)
	j done_label1
	nop
false_label1:
	addi $t5, $zero, 70
	sw $t5, 12($t9)
done_label1:
	lw $t0, 4($t9)
	addi $t4, $zero, 1
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t6, $zero, $t0
	lw $t0, 0($t9)
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
	addi $t5, $zero, 84
	sw $t5, 16($t9)
	j done_label2
	nop
false_label2:
	addi $t5, $zero, 70
	sw $t5, 16($t9)
done_label2:
