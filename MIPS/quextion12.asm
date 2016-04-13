.text
main:
	li $a0, 4
	li $a1, 2
	li $a2, 7
	add $a1, $a0, $a1
	beqz $a1,l1
	mul $a0, $a0, $a1
l1:
	beqz $a0, l2
	mul $a2, $a2, 3
l2:
	add $a2, $a2, $a2
	li $v0, 10
	syscall
.data

bgt 