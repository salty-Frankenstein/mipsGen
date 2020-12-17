	addi $t9, $zero, 268500992
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
	lw $t0, 0($t9)
	slt $t0, $t0, $t2
	blez $t0 false_label2
	nop
false_label2:
	nop
done_label2:
done_label0:
	lw $t0, 0($t9)
	slt $t0, $t0, $t1
	blez $t0 false_label3
	nop
false_label3:
	nop
done_label3:
	lw $t0, 0($t9)
	slt $t0, $t0, $t1
	blez $t0 false_label4
	nop
false_label4:
	nop
done_label4:
