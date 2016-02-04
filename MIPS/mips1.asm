.text
main:
	li $t1, 4
	li $t2, 6
	add $a0, $t1, $t2
	li $v0, 1
	syscall
	
	li $t3, 21
	li $t4, 2
	mul $a0, $t3, $t4
	li $v0, 1
	syscall
	
	l
	
.data


.text
	li $a0, 4
	li $a1, 7
	li $a2, 2
	div $a1, $a1, $a2
	add $a0, $a1, $a1
	li $v0, 1
	syscall
	
.data