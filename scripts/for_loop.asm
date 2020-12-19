	addi $t9, $zero, 268697600
	addi $t0, $zero, 50
	sw $t0, 0($t9)
	addi $t0, $zero, 0
	sw $t0, 4($t9)
	addi $t0, $zero, 0
	sw $t0, 8($t9)
	lw $t0, 8($t9)
	slti $t0, $t0, 50
	blez $t0 done0
loop0:
	addi $t0, $zero, 0
	sw $t0, 12($t9)
	lw $t0, 12($t9)
	slti $t0, $t0, 50
	blez $t0 done1
loop1:
	lw $t0, 8($t9)
	addu $t1, $zero, $t0
	lw $t0, 12($t9)
	slt $t0, $t0, $t1
	blez $t0 false_label2
	lw $t0, 4($t9)
	addi $t0, $t0, 1
	sw $t0, 4($t9)
	j done_label2
	nop
false_label2:
	lw $t0, 4($t9)
	addi $t0, $t0, 1
	sw $t0, 4($t9)
	lw $t0, 4($t9)
	addi $t0, $t0, 1
	sw $t0, 4($t9)
done_label2:
	lw $t0, 12($t9)
	addi $t0, $t0, 1
	sw $t0, 12($t9)
	lw $t0, 12($t9)
	slti $t0, $t0, 50
	xori $t0, $t0, 1
	blez $t0 loop1
done1:
	lw $t0, 8($t9)
	addi $t0, $t0, 1
	sw $t0, 8($t9)
	lw $t0, 8($t9)
	slti $t0, $t0, 50
	xori $t0, $t0, 1
	blez $t0 loop0
done0:
	addi $t0, $zero, 0
	sw $t0, 12($t9)
	addi $t0, $zero, 0
	sw $t0, 8($t9)
loop3:
	addi $t0, $zero, 0
	sw $t0, 16($t9)
loop4:
	lw $t0, 8($t9)
	addu $t1, $zero, $t0
	lw $t0, 16($t9)
	slt $t0, $t0, $t1
	blez $t0 false_label5
	lw $t0, 12($t9)
	addi $t0, $t0, 1
	sw $t0, 12($t9)
	j done_label5
	nop
false_label5:
	lw $t0, 12($t9)
	addi $t0, $t0, 1
	sw $t0, 12($t9)
	lw $t0, 12($t9)
	addi $t0, $t0, 1
	sw $t0, 12($t9)
done_label5:
	lw $t0, 16($t9)
	addi $t0, $t0, 1
	sw $t0, 16($t9)
	lw $t0, 16($t9)
	slti $t0, $t0, 50
	xori $t0, $t0, 1
	blez $t0 loop4
	lw $t0, 8($t9)
	addi $t0, $t0, 1
	sw $t0, 8($t9)
	lw $t0, 8($t9)
	slti $t0, $t0, 50
	xori $t0, $t0, 1
	blez $t0 loop3
