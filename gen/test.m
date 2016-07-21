.text
.globl main
fib:
	addiu $sp,$sp,-44
	sw $fp,0($sp)
	sw $ra,4($sp)
	addiu $fp,$sp,40
	sw $a0,-8($fp)
	li $t0,2
	sw $t0,-12($fp)
	lw $t0,-8($fp)
	lw $t1,-12($fp)
	slt $t0,$t0,$t1
	sw $t0,-4($fp)
	lw $t0,-4($fp)
	beqz $t0,L1
	b L0
	L0:
	sw $a0,-8($fp)
	lw $v0,-8($fp)
	lw $fp,0($sp)
	lw $ra,4($sp)
	addiu $sp,$sp,44
	jr $ra
	b L2
	L1:
	sw $a0,-24($fp)
	li $t0,1
	sw $t0,-28($fp)
	lw $t0,-24($fp)
	lw $t1,-28($fp)
	sub $t0,$t0,$t1
	sw $t0,-20($fp)
	lw $a0,-20($fp)
	lw $t0,-20($fp)
	sw $t0,-4($sp)
	jal fib
	lw $a0,0($fp)
	sw $v0,-12($fp)
	sw $a0,-24($fp)
	li $t0,2
	sw $t0,-28($fp)
	lw $t0,-24($fp)
	lw $t1,-28($fp)
	sub $t0,$t0,$t1
	sw $t0,-20($fp)
	lw $a0,-20($fp)
	lw $t0,-20($fp)
	sw $t0,-4($sp)
	jal fib
	lw $a0,0($fp)
	sw $v0,-16($fp)
	lw $t0,-12($fp)
	lw $t1,-16($fp)
	add $t0,$t0,$t1
	sw $t0,-8($fp)
	lw $v0,-8($fp)
	lw $fp,0($sp)
	lw $ra,4($sp)
	addiu $sp,$sp,44
	jr $ra
	b L2
	L2:
	lw $fp,0($sp)
	lw $ra,4($sp)
	addiu $sp,$sp,44
	jr $ra
main:
	addiu $sp,$sp,-20
	sw $fp,0($sp)
	sw $ra,4($sp)
	addiu $fp,$sp,20
	li $t0,6
	sw $t0,-8($fp)
	lw $a0,-8($fp)
	lw $t0,-8($fp)
	sw $t0,-4($sp)
	jal fib
	lw $a0,0($fp)
	sw $v0,-4($fp)
	move $t3,$a0
	lw $a0,-4($fp)
	li $v0,1
	syscall
	li $v0,4
	la $a0,newline
	syscall
	move $a0,$t3
	lw $fp,0($sp)
	lw $ra,4($sp)
	addiu $sp,$sp,20
	jr $ra
.data
	newline: .asciiz "\n"
	
