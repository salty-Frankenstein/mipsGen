	addi $fp, $zero, 268697596
	addi $sp, $zero, 268697596
	j main
main:
	addi $sp, $sp, 4
	addi $sp, $sp, 4
	addi $t5, $zero, 50
	sw $t5, 4($fp)
	addi $t5, $zero, 0
	sw $t5, 8($fp)
	addi $sp, $sp, 4
	addi $t5, $zero, 0
	sw $t5, 12($fp)
	lw $t0, 12($fp)
	slti $t0, $t0, 50
	blez $t0 done0
	nop
loop0:
	addi $sp, $sp, 4
	addi $t5, $zero, 0
	sw $t5, 16($fp)
	lw $t0, 16($fp)
	slti $t0, $t0, 50
	blez $t0 done1
	nop
loop1:
	lw $t0, 12($fp)
	addu $t1, $zero, $t0
	lw $t0, 16($fp)
	slt $t0, $t0, $t1
	blez $t0 false_label2
	nop
	lw $t0, 8($fp)
	addi $t0, $t0, 1
	sw $t0, 8($fp)
	j done_label2
	nop
false_label2:
	lw $t0, 8($fp)
	addi $t0, $t0, 1
	sw $t0, 8($fp)
	lw $t0, 8($fp)
	addi $t0, $t0, 1
	sw $t0, 8($fp)
done_label2:
	lw $t0, 16($fp)
	addi $t0, $t0, 1
	sw $t0, 16($fp)
	lw $t0, 16($fp)
	slti $t0, $t0, 50
	xori $t0, $t0, 1
	blez $t0 loop1
	nop
done1:
	lw $t0, 12($fp)
	addi $t0, $t0, 1
	sw $t0, 12($fp)
	lw $t0, 12($fp)
	slti $t0, $t0, 50
	xori $t0, $t0, 1
	blez $t0 loop0
	nop
done0:
	addi $sp, $sp, 4
	addi $t5, $zero, 0
	sw $t5, 20($fp)
	addi $t5, $zero, 0
	sw $t5, 12($fp)
loop3:
	addi $sp, $sp, 4
	addi $t5, $zero, 0
	sw $t5, 24($fp)
loop4:
	lw $t0, 12($fp)
	addu $t1, $zero, $t0
	lw $t0, 24($fp)
	slt $t0, $t0, $t1
	blez $t0 false_label5
	nop
	lw $t0, 20($fp)
	addi $t0, $t0, 1
	sw $t0, 20($fp)
	j done_label5
	nop
false_label5:
	lw $t0, 20($fp)
	addi $t0, $t0, 1
	sw $t0, 20($fp)
	lw $t0, 20($fp)
	addi $t0, $t0, 1
	sw $t0, 20($fp)
done_label5:
	lw $t0, 24($fp)
	addi $t0, $t0, 1
	sw $t0, 24($fp)
	lw $t0, 24($fp)
	slti $t0, $t0, 50
	xori $t0, $t0, 1
	blez $t0 loop4
	nop
	lw $t0, 12($fp)
	addi $t0, $t0, 1
	sw $t0, 12($fp)
	lw $t0, 12($fp)
	slti $t0, $t0, 50
	xori $t0, $t0, 1
	blez $t0 loop3
	nop
