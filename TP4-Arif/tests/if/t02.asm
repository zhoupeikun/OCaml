.text
#Code principal
main:
	li $v0, 235710
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
	lw $zero, 0($sp)
	add $sp, $sp, 4
	sub $sp, $sp, 4
	sw $zero, 0($sp)
	jal print_newline
	lw $zero, 0($sp)
	add $sp, $sp, 4
	li $v0, 25
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	li $v0, 12
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $v0, 0($sp)
	add $sp, $sp, 4
	lw $v1, 0($sp)
	add $sp, $sp, 4
	seq $v0, $v0, $v1
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $a0, 0($sp)
	add $sp, $sp, 4
	beqz $a0, __label__00002
	li $v0, 1
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
	b __label__00001
__label__00002:
	li $v0, 2
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
__label__00001:
	lw $zero, 0($sp)
	add $sp, $sp, 4
	li $v0, 25
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	li $v0, 12
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $v0, 0($sp)
	add $sp, $sp, 4
	lw $v1, 0($sp)
	add $sp, $sp, 4
	sne $v0, $v0, $v1
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $a0, 0($sp)
	add $sp, $sp, 4
	beqz $a0, __label__00004
	li $v0, 3
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
	b __label__00003
__label__00004:
	li $v0, 4
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
__label__00003:
	lw $zero, 0($sp)
	add $sp, $sp, 4
	li $v0, 25
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	li $v0, 12
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $v0, 0($sp)
	add $sp, $sp, 4
	lw $v1, 0($sp)
	add $sp, $sp, 4
	slt $v0, $v0, $v1
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $a0, 0($sp)
	add $sp, $sp, 4
	beqz $a0, __label__00006
	li $v0, 5
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
	b __label__00005
__label__00006:
	li $v0, 6
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
__label__00005:
	lw $zero, 0($sp)
	add $sp, $sp, 4
	li $v0, 25
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	li $v0, 25
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $v0, 0($sp)
	add $sp, $sp, 4
	lw $v1, 0($sp)
	add $sp, $sp, 4
	sle $v0, $v0, $v1
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $a0, 0($sp)
	add $sp, $sp, 4
	beqz $a0, __label__00008
	li $v0, 7
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
	b __label__00007
__label__00008:
	li $v0, 8
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
__label__00007:
	lw $zero, 0($sp)
	add $sp, $sp, 4
	li $v0, 25
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	li $v0, 12
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $v0, 0($sp)
	add $sp, $sp, 4
	lw $v1, 0($sp)
	add $sp, $sp, 4
	sge $v0, $v0, $v1
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	lw $a0, 0($sp)
	add $sp, $sp, 4
	beqz $a0, __label__00010
	li $v0, 9
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
	b __label__00009
__label__00010:
	li $v0, 10
	sub $sp, $sp, 4
	sw $v0, 0($sp)
	jal print_int
__label__00009:
	lw $zero, 0($sp)
	add $sp, $sp, 4
#Fin
end_exec:
	li $v0, 10
	syscall
#Primitives
print_newline:
	lw $zero, 0($sp)
	add $sp, $sp, 4
	li $v0, 11
	li $a0, 10
	syscall
	sub $sp, $sp, 4
	sw $zero, 0($sp)
	jr $ra
print_int:
	lw $a0, 0($sp)
	add $sp, $sp, 4
	li $v0, 1
	syscall
	sub $sp, $sp, 4
	sw $zero, 0($sp)
	jr $ra
.data
