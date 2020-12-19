	addi $t9, $zero, 268697600
	addi $t5, $zero, 0
	sw $t5, 48($t9)
	lw $t0, 48($t9)
	slti $t0, $t0, 3
	blez $t0 done0
loop0:
	addi $t5, $zero, 0
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 0($t0)
	addi $t5, $zero, 0
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 16($t0)
	addi $t5, $zero, 0
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 32($t0)
	lw $t0, 48($t9)
	addi $t0, $t0, 1
	sw $t0, 48($t9)
	lw $t0, 48($t9)
	slti $t0, $t0, 3
	xori $t0, $t0, 1
	blez $t0 loop0
done0:
	addi $t5, $zero, 7
	sw $t5, 0($t9)
	addi $t5, $zero, 1
	sw $t5, 4($t9)
	addi $t5, $zero, 2
	sw $t5, 12($t9)
	addi $t5, $zero, 8
	sw $t5, 16($t9)
	addi $t5, $zero, 1
	sw $t5, 28($t9)
	addi $t5, $zero, 0
	sw $t5, 52($t9)
	lw $t0, 28($t9)
	addu $t1, $zero, $t0
	lw $t0, 12($t9)
	slt $t0, $t0, $t1
	blez $t0 false_label1
	lw $t0, 28($t9)
	addu $t5, $zero, $t0
	sw $t5, 56($t9)
	j done_label1
	nop
false_label1:
	lw $t0, 12($t9)
	addu $t5, $zero, $t0
	sw $t5, 56($t9)
done_label1:
	addi $t5, $zero, 0
	sw $t5, 48($t9)
	lw $t0, 56($t9)
	addu $t1, $zero, $t0
	lw $t0, 48($t9)
	slt $t0, $t0, $t1
	blez $t0 done2
loop2:
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 16($t0)
	addu $t1, $zero, $t0
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 0($t0)
	addu $t0, $t0, $t1
	addu $t5, $zero, $t0
	sw $t5, 60($t9)
	lw $t0, 52($t9)
	addu $t1, $zero, $t0
	lw $t0, 60($t9)
	addu $t0, $t0, $t1
	addu $t5, $zero, $t0
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 32($t0)
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	slti $t0, $t0, 10
	blez $t0 false_label3
	addi $t5, $zero, 0
	sw $t5, 52($t9)
	j done_label3
	nop
false_label3:
	addi $t5, $zero, 1
	sw $t5, 52($t9)
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addiu $t0, $t0, -10
	addu $t5, $zero, $t0
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 32($t0)
done_label3:
	lw $t0, 48($t9)
	addi $t0, $t0, 1
	sw $t0, 48($t9)
	lw $t0, 56($t9)
	addu $t1, $zero, $t0
	lw $t0, 48($t9)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	blez $t0 loop2
done2:
	lw $t0, 52($t9)
	blez $t0 false_label4
	lw $t0, 52($t9)
	addu $t5, $zero, $t0
	lw $t0, 48($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 32($t0)
	lw $t0, 48($t9)
	addi $t0, $t0, 1
	sw $t0, 48($t9)
	j done_label4
	nop
false_label4:
	nop
done_label4:
	lw $t0, 48($t9)
	addu $t5, $zero, $t0
	sw $t5, 44($t9)
