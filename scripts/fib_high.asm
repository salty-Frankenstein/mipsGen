	addi $fp, $zero, 268697600
	addi $sp, $zero, 268697600
	addi $t5, $zero, 100
	sw $t5, 0($fp)
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
	addi $t5, $zero, 1
	sw $t5, 4($fp)
	addi $t5, $zero, 1
	sw $t5, 404($fp)
	addi $t5, $zero, 1
	sw $t5, 408($fp)
	addi $t5, $zero, 1
	sw $t5, 808($fp)
	addi $t5, $zero, 1
	sw $t5, 1216($fp)
	lw $t0, 0($fp)
	addu $t1, $zero, $t0
	lw $t0, 1216($fp)
	slt $t0, $t0, $t1
	blez $t0 done1
	nop
loop1:
	addi $t5, $zero, 0
	sw $t5, 1220($fp)
	lw $t0, 808($fp)
	addu $t1, $zero, $t0
	lw $t0, 404($fp)
	slt $t0, $t0, $t1
	blez $t0 false_label2
	nop
	lw $t0, 808($fp)
	addu $t5, $zero, $t0
	sw $t5, 1224($fp)
	j done_label2
	nop
false_label2:
	lw $t0, 404($fp)
	addu $t5, $zero, $t0
	sw $t5, 1224($fp)
done_label2:
	addi $t5, $zero, 0
	sw $t5, 1228($fp)
	lw $t0, 1224($fp)
	addu $t1, $zero, $t0
	lw $t0, 1228($fp)
	slt $t0, $t0, $t1
	blez $t0 done3
	nop
loop3:
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 408($t0)
	addu $t1, $zero, $t0
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 4($t0)
	addu $t0, $t0, $t1
	addu $t5, $zero, $t0
	sw $t5, 1232($fp)
	lw $t0, 1220($fp)
	addu $t1, $zero, $t0
	lw $t0, 1232($fp)
	addu $t0, $t0, $t1
	addu $t5, $zero, $t0
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 812($t0)
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 812($t0)
	slti $t0, $t0, 10
	blez $t0 false_label4
	nop
	addi $t5, $zero, 0
	sw $t5, 1220($fp)
	j done_label4
	nop
false_label4:
	addi $t5, $zero, 1
	sw $t5, 1220($fp)
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 812($t0)
	addiu $t0, $t0, -10
	addu $t5, $zero, $t0
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 812($t0)
done_label4:
	lw $t0, 1228($fp)
	addi $t0, $t0, 1
	sw $t0, 1228($fp)
	lw $t0, 1224($fp)
	addu $t1, $zero, $t0
	lw $t0, 1228($fp)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	blez $t0 loop3
	nop
done3:
	lw $t0, 1220($fp)
	blez $t0 false_label5
	nop
	lw $t0, 1220($fp)
	addu $t5, $zero, $t0
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 812($t0)
	lw $t0, 1228($fp)
	addi $t0, $t0, 1
	sw $t0, 1228($fp)
	j done_label5
	nop
false_label5:
	nop
done_label5:
	lw $t0, 1228($fp)
	addu $t5, $zero, $t0
	sw $t5, 1212($fp)
	addi $t5, $zero, 0
	sw $t5, 1228($fp)
	lw $t0, 808($fp)
	addu $t1, $zero, $t0
	lw $t0, 1228($fp)
	slt $t0, $t0, $t1
	blez $t0 done6
	nop
loop6:
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 408($t0)
	addu $t5, $zero, $t0
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 4($t0)
	lw $t0, 1228($fp)
	addi $t0, $t0, 1
	sw $t0, 1228($fp)
	lw $t0, 808($fp)
	addu $t1, $zero, $t0
	lw $t0, 1228($fp)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	blez $t0 loop6
	nop
done6:
	lw $t0, 808($fp)
	addu $t5, $zero, $t0
	sw $t5, 404($fp)
	addi $t5, $zero, 0
	sw $t5, 1228($fp)
	lw $t0, 1212($fp)
	addu $t1, $zero, $t0
	lw $t0, 1228($fp)
	slt $t0, $t0, $t1
	blez $t0 done7
	nop
loop7:
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t0, $t0, $fp
	lw $t0, 812($t0)
	addu $t5, $zero, $t0
	lw $t0, 1228($fp)
	sll $t0, $t0, 2
	addu $t3, $zero, $t0
	addu $t0, $t3, $fp
	sw $t5, 408($t0)
	lw $t0, 1228($fp)
	addi $t0, $t0, 1
	sw $t0, 1228($fp)
	lw $t0, 1212($fp)
	addu $t1, $zero, $t0
	lw $t0, 1228($fp)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	blez $t0 loop7
	nop
done7:
	lw $t0, 1212($fp)
	addu $t5, $zero, $t0
	sw $t5, 808($fp)
	lw $t0, 1216($fp)
	addi $t0, $t0, 1
	sw $t0, 1216($fp)
	lw $t0, 0($fp)
	addu $t1, $zero, $t0
	lw $t0, 1216($fp)
	slt $t0, $t0, $t1
	xori $t0, $t0, 1
	blez $t0 loop1
	nop
done1:
