#ex4
.text

main:
	add $sp, $sp, -8 
	li $a0, 36
	li $a1, 6
	sw $a0, 0($sp) 
	sw $a1, 4($sp)
	jal addLast
	li $v0, 10
	syscall
	
addLast:
	lw $t0, 0($sp)
	lw $t1, 4($sp) 
	addi $sp, $sp, 8
	add $t0, $t0, $t1
	addi, $sp, $sp, -4 
	sw $t0, 0($sp)
	jr $ra
	
.data
