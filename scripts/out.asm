	addi $t9, $zero, 0
	lw $t0, 0($t9)
	slt $t0, $t0, $t1
	blez $t0 false_label0
	lw $t0, 4($t9)
	slt $t0, $t0, $t2
	blez $t0 false_label1
	nop
false_label1:
	nop
done_label1:
false_label0:
	nop
done_label0:
	lw $t0, 0($t9)
	slt $t0, $t0, $t1
	blez $t0 false_label1
	nop
false_label1:
	nop
done_label1:
	lw $t0, 0($t9)
	slt $t0, $t0, $t1
	blez $t0 false_label2
	nop
false_label2:
	nop
done_label2:
