#ex2.1
.text
	li $t1, 1
	li $t2, 0
	and $a0, $t1, $t2
	li $v0, 1
	syscall
.data

#ex2.2
.text
	li $t1, 3
	li $t2, 4
	li $t3, 10
	li $t4, 2
	bne $t1, $t2, goto
	li $a0, 14
	li $v0, 1
	syscall
	
goto:
	mul $a0, $t3, $t4
	li $v0, 1
	syscall
	
.data

#ex2.3
.text
	li $t1, 2
	li $t2, 3
	li $t3, 4
	li $t4, 2
	li $t5, 3
	beq $t1, $t2, goto2
	mul $t4, $t4, $t5
	sub $t3, $t3, $t4
	blez $t3, goto2
	li $a0, 0
	jal print_int
	li $v0, 10
	syscall
		
goto2:
	li $a0, 1
	jal print_int
	li $v0, 10
	syscall
	
print_int:
	li $v0, 1
	syscall
	jr $ra
	
