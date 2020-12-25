	addi $fp, $zero, 268697596
	addi $sp, $zero, 268697596
	j main
main:
	addi $sp, $sp, 400
	addi $sp, $sp, 4
	addi $sp, $sp, 400
	addi $sp, $sp, 4
	addi $sp, $sp, 400
	addi $sp, $sp, 4
	addi $sp, $sp, 4
	addi $t5, $zero, 0
	sw $t5, 1216($fp)
	lw $t0, 1216($fp)
	slti $t0, $t0, 100
	blez $t0 done0
	nop
loop0:
	addi $t5, $zero, 0
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 4($t0)
	addi $t5, $zero, 0
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 408($t0)
	addi $t5, $zero, 0
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 812($t0)
	lw $t0, 1216($fp)
	addi $t0, $t0, 1
	sw $t0, 1216($fp)
	lw $t0, 1216($fp)
	slti $t0, $t0, 100
	xori $t0, $t0, 1
	blez $t0 loop0
	nop
done0:
	addi $t5, $zero, 7
	sw $t5, 4($fp)
	addi $t5, $zero, 1
	sw $t5, 8($fp)
	addi $t5, $zero, 2
	sw $t5, 404($fp)
	addi $t5, $zero, 8
	sw $t5, 408($fp)
	addi $t5, $zero, 1
	sw $t5, 808($fp)
	addi $sp, $sp, 4
	addi $t5, $zero, 0
	sw $t5, 1220($fp)
	addi $sp, $sp, 4
	lw $t0, 808($fp)
	addu $t1, $zero, $t0
	lw $t0, 404($fp)
	slt $t0, $t0, $t1
	blez $t0 false_label1
	nop
	lw $t0, 808($fp)
	addu $t5, $zero, $t0
	sw $t5, 1224($fp)
	j done_label1
	nop
false_label1:
	lw $t0, 404($fp)
	addu $t5, $zero, $t0
	sw $t5, 1224($fp)
done_label1:
	addi $t5, $zero, 0
	sw $t5, 1216($fp)
	lw $t0, 1224($fp)
	addu $t1, $zero, $t0
	lw $t0, 1216($fp)
	slt $t0, $t0, $t1
	blez $t0 done2
	nop
loop2:
	addi $sp, $sp, 4
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 408($t0)
	addu $t1, $zero, $t0
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 4($t0)
	addu $t0, $t0, $t1
	addu $t5, $zero, $t0
	sw $t5, 1228($fp)
	lw $t0, 1220($fp)
	addu $t1, $zero, $t0
	lw $t0, 1228($fp)
	addu $t0, $t0, $t1
	addu $t5, $zero, $t0
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 812($t0)
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 812($t0)
	slti $t0, $t0, 10
	blez $t0 false_label3
	nop
	addi $t5, $zero, 0
	sw $t5, 1220($fp)
	j done_label3
	nop
false_label3:
	addi $t5, $zero, 1
	sw $t5, 1220($fp)
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 812($t0)
	addiu $t0, $t0, -10
	addu $t5, $zero, $t0
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 812($t0)
done_label3:
	lw $t0, 1216($fp)
	addi $t0, $t0, 1
	sw $t0, 1216($fp)
	lw $t0, 1224($fp)
	addu $t1, $zero, $t0
	lw $t0, 1216($fp)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	blez $t0 loop2
	nop
done2:
	lw $t0, 1220($fp)
	blez $t0 false_label4
	nop
	lw $t0, 1220($fp)
	addu $t5, $zero, $t0
	lw $t0, 1216($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 812($t0)
	lw $t0, 1216($fp)
	addi $t0, $t0, 1
	sw $t0, 1216($fp)
	j done_label4
	nop
false_label4:
	nop
done_label4:
	lw $t0, 1216($fp)
	addu $t5, $zero, $t0
	sw $t5, 1212($fp)
