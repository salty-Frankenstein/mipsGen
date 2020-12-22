	addi $t9, $zero, 268697600
	addi $t5, $zero, 40
	sw $t5, 32($t9)
	addi $t5, $zero, 57
	sw $t5, 36($t9)
	addi $t5, $zero, 43
	sw $t5, 40($t9)
	addi $t5, $zero, 54
	sw $t5, 44($t9)
	addi $t5, $zero, 42
	sw $t5, 48($t9)
	addi $t5, $zero, 54
	sw $t5, 52($t9)
	addi $t5, $zero, 41
	sw $t5, 56($t9)
	addi $t5, $zero, 45
	sw $t5, 60($t9)
	addi $t5, $zero, 51
	sw $t5, 64($t9)
	addi $t5, $zero, 48
	sw $t5, 544($t9)
	addi $t5, $zero, 57
	sw $t5, 548($t9)
	addi $t5, $zero, 0
	sw $t5, 412($t9)
	addi $t5, $zero, 0
	sw $t5, 284($t9)
	addi $t5, $zero, 0
	sw $t5, 552($t9)
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	blez $t0 done0
	nop
loop0:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addu $t1, $zero, $t0
	lw $t0, 544($t9)
	slt $t0, $t0, $t1
	addu $t6, $zero, $t0
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addu $t1, $zero, $t0
	lw $t0, 548($t9)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	addu $t7, $zero, $t0
	xori $t1, $t6, 1
	xori $t2, $t7, 1
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label1
	nop
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addu $t5, $zero, $t0
	lw $t0, 284($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 160($t0)
	lw $t0, 284($t9)
	addi $t0, $t0, 1
	sw $t0, 284($t9)
	j done_label1
	nop
false_label1:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addi $t4, $zero, 40
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label2
	nop
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addu $t5, $zero, $t0
	lw $t0, 412($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 288($t0)
	lw $t0, 412($t9)
	addi $t0, $t0, 1
	sw $t0, 412($t9)
	j done_label2
	nop
false_label2:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addi $t4, $zero, 41
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label3
	nop
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addi $t4, $zero, 40
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	blez $t0 done4
	nop
loop4:
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addu $t5, $zero, $t0
	lw $t0, 284($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 160($t0)
	lw $t0, 284($t9)
	addi $t0, $t0, 1
	sw $t0, 284($t9)
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	addu $t5, $zero, $t0
	sw $t5, 412($t9)
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addi $t4, $zero, 40
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	xori $t0, $t0, 1
	blez $t0 loop4
	nop
done4:
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	addu $t5, $zero, $t0
	sw $t5, 412($t9)
	j done_label3
	nop
false_label3:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addi $t4, $zero, 42
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	addu $t6, $zero, $t0
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addi $t4, $zero, 42
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	addu $t7, $zero, $t0
	xori $t1, $t6, 1
	xori $t2, $t7, 1
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label5
	nop
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addu $t5, $zero, $t0
	lw $t0, 412($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 288($t0)
	lw $t0, 412($t9)
	addi $t0, $t0, 1
	sw $t0, 412($t9)
	j done_label5
	nop
false_label5:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addi $t4, $zero, 42
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label6
	nop
	lw $t0, 412($t9)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	addu $t6, $zero, $t0
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addi $t4, $zero, 42
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
	blez $t0 done7
	nop
loop7:
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addu $t5, $zero, $t0
	lw $t0, 284($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 160($t0)
	lw $t0, 284($t9)
	addi $t0, $t0, 1
	sw $t0, 284($t9)
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	addu $t5, $zero, $t0
	sw $t5, 412($t9)
	lw $t0, 412($t9)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	addu $t6, $zero, $t0
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addi $t4, $zero, 42
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
	xori $t0, $t0, 1
	blez $t0 loop7
	nop
done7:
	j done_label6
	nop
false_label6:
	lw $t0, 412($t9)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	addu $t6, $zero, $t0
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addi $t4, $zero, 40
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	addu $t7, $zero, $t0
	xori $t1, $t6, 1
	xori $t2, $t7, 1
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 done8
	nop
loop8:
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addu $t5, $zero, $t0
	lw $t0, 284($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 160($t0)
	lw $t0, 284($t9)
	addi $t0, $t0, 1
	sw $t0, 284($t9)
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	addu $t5, $zero, $t0
	sw $t5, 412($t9)
	lw $t0, 412($t9)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	addu $t6, $zero, $t0
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addi $t4, $zero, 40
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	addu $t7, $zero, $t0
	xori $t1, $t6, 1
	xori $t2, $t7, 1
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	blez $t0 loop8
	nop
done8:
done_label6:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addu $t5, $zero, $t0
	lw $t0, 412($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 288($t0)
	lw $t0, 412($t9)
	addi $t0, $t0, 1
	sw $t0, 412($t9)
done_label5:
done_label3:
done_label2:
done_label1:
	lw $t0, 552($t9)
	addi $t0, $t0, 1
	sw $t0, 552($t9)
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 32($t0)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	xori $t0, $t0, 1
	blez $t0 loop0
	nop
done0:
	lw $t0, 412($t9)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	blez $t0 done9
	nop
loop9:
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 288($t0)
	addu $t5, $zero, $t0
	lw $t0, 284($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 160($t0)
	lw $t0, 284($t9)
	addi $t0, $t0, 1
	sw $t0, 284($t9)
	lw $t0, 412($t9)
	addiu $t0, $t0, -1
	addu $t5, $zero, $t0
	sw $t5, 412($t9)
	lw $t0, 412($t9)
	addi $t4, $zero, 0
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	xori $t0, $t0, 1
	xori $t0, $t0, 1
	blez $t0 loop9
	nop
done9:
	addi $t5, $zero, 0
	sw $t5, 540($t9)
	addi $t5, $zero, 0
	sw $t5, 552($t9)
	lw $t0, 284($t9)
	addu $t1, $zero, $t0
	lw $t0, 552($t9)
	slt $t0, $t0, $t1
	blez $t0 done10
	nop
loop10:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 160($t0)
	addu $t1, $zero, $t0
	lw $t0, 544($t9)
	slt $t0, $t0, $t1
	addu $t6, $zero, $t0
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 160($t0)
	addu $t1, $zero, $t0
	lw $t0, 548($t9)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	addu $t7, $zero, $t0
	xori $t1, $t6, 1
	xori $t2, $t7, 1
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label11
	nop
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 160($t0)
	addiu $t0, $t0, -48
	addu $t5, $zero, $t0
	lw $t0, 540($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 416($t0)
	lw $t0, 540($t9)
	addi $t0, $t0, 1
	sw $t0, 540($t9)
	j done_label11
	nop
false_label11:
	lw $t0, 540($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 416($t0)
	addu $t5, $zero, $t0
	sw $t5, 560($t9)
	lw $t0, 540($t9)
	addiu $t0, $t0, -1
	addu $t5, $zero, $t0
	sw $t5, 540($t9)
	lw $t0, 540($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 416($t0)
	addu $t5, $zero, $t0
	sw $t5, 556($t9)
	lw $t0, 540($t9)
	addiu $t0, $t0, -1
	addu $t5, $zero, $t0
	sw $t5, 540($t9)
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 160($t0)
	addi $t4, $zero, 43
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label12
	nop
	lw $t0, 560($t9)
	addu $t1, $zero, $t0
	lw $t0, 556($t9)
	addu $t0, $t0, $t1
	addu $t5, $zero, $t0
	lw $t0, 540($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 416($t0)
	lw $t0, 540($t9)
	addi $t0, $t0, 1
	sw $t0, 540($t9)
	j done_label12
	nop
false_label12:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 160($t0)
	addi $t4, $zero, 45
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label13
	nop
	lw $t0, 556($t9)
	addu $t6, $zero, $t0
	lw $t0, 560($t9)
	addu $t7, $zero, $t0
	subu $t0, $t6, $t7
	addu $t5, $zero, $t0
	lw $t0, 540($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 416($t0)
	lw $t0, 540($t9)
	addi $t0, $t0, 1
	sw $t0, 540($t9)
	j done_label13
	nop
false_label13:
	lw $t0, 552($t9)
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 160($t0)
	addi $t4, $zero, 42
	slt $t1, $t0, $t4
	slt $t2, $t4, $t0
	nor $t0, $t1, $t2
	sll $t0, $t0, 31
	srl $t0, $t0, 31
	blez $t0 false_label14
	nop
	lw $t0, 556($t9)
	addu $t6, $zero, $t0
	lw $t0, 560($t9)
	addu $t7, $zero, $t0
	mul $t0, $t6, $t7
	addu $t5, $zero, $t0
	lw $t0, 540($t9)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $t9
	sw $t5, 416($t0)
	lw $t0, 540($t9)
	addi $t0, $t0, 1
	sw $t0, 540($t9)
	j done_label14
	nop
false_label14:
	nop
done_label14:
done_label13:
done_label12:
done_label11:
	lw $t0, 552($t9)
	addi $t0, $t0, 1
	sw $t0, 552($t9)
	lw $t0, 284($t9)
	addu $t1, $zero, $t0
	lw $t0, 552($t9)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	blez $t0 loop10
	nop
done10:
	lw $t0, 540($t9)
	addiu $t0, $t0, -1
	sll $t0, $t0, 2
	addu $t0, $t0, $t9
	lw $t0, 416($t0)
	addu $t5, $zero, $t0
	sw $t5, 0($t9)
