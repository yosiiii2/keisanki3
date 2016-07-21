.text
.globl main
main:
	addiu $sp,$sp,-44
	sw $fp,0($sp)
	sw $ra,4($sp)
	addiu $fp,$sp,44
	li $t0,3
	sw $t0,-20($fp)
	li $t0,3
	sw $t0,-4($fp)
	li $t0,3
	sw $t0,-24($fp)
	li $t0,7
	sw $t0,-28($fp)
	li $t0,10
	sw $t0,-20($fp)
	li $t0,10
	sw $t0,-8($fp)
	li $t0,0
	sw $t0,-20($fp)
	li $t0,0
	sw $t0,-12($fp)
	li $t0,0
	sw $t0,-20($fp)
	li $t0,0
	sw $t0,-16($fp)
	L0:
	lw $t0,-16($fp)
	sw $t0,-24($fp)
	li $t0,10
	sw $t0,-28($fp)
	lw $t0,-24($fp)
	lw $t1,-28($fp)
	slt $t0,$t0,$t1
	sw $t0,-20($fp)
	lw $t0,-20($fp)
	beqz $t0,L2
	b L1
	L1:
	lw $t0,-12($fp)
	sw $t0,-28($fp)
	lw $t0,-16($fp)
	sw $t0,-32($fp)
	lw $t0,-28($fp)
	lw $t1,-32($fp)
	add $t0,$t0,$t1
	sw $t0,-24($fp)
	lw $t0,-24($fp)
	sw $t0,-12($fp)
	lw $t0,-12($fp)
	sw $t0,-24($fp)
	move $t3,$a0
	lw $a0,-24($fp)
	li $v0,1
	syscall
	li $v0,4
	la $a0,newline
	syscall
	move $a0,$t3
	lw $t0,-16($fp)
	sw $t0,-28($fp)
	li $t0,1
	sw $t0,-32($fp)
	lw $t0,-28($fp)
	lw $t1,-32($fp)
	add $t0,$t0,$t1
	sw $t0,-24($fp)
	lw $t0,-24($fp)
	sw $t0,-16($fp)
	b L0
	L2:
	lw $t0,-8($fp)
	sw $t0,-20($fp)
	move $t3,$a0
	lw $a0,-20($fp)
	li $v0,1
	syscall
	li $v0,4
	la $a0,newline
	syscall
	move $a0,$t3
	li $t0,0
	sw $t0,-20($fp)
	lw $v0,-20($fp)
	lw $fp,0($sp)
	lw $ra,4($sp)
	addiu $sp,$sp,44
	jr $ra
	lw $fp,0($sp)
	lw $ra,4($sp)
	addiu $sp,$sp,44
	jr $ra
.data
	newline: .asciiz "\n"
	