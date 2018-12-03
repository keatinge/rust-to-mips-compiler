
### BEGIN STD LIB ###
mips_mul:
mult $a0, $a1
mflo $v0
jr $ra


refresh_ra:
jr $ra

mips_add:
add $v0, $a0, $a1
jr $ra

mips_print_i32:
li $v0, 1
syscall
li $a0, 10
li $v0, 11
syscall
jr $ra


mips_exit:
li $v0, 10
syscall
jr $ra


mips_rem:
div $a0, $a1
mfhi $v0
jr $ra

####  END STD LIB ###
auto_generated_if_2:
li $t2, 0
jr $ra


auto_generated_loop_1:
beq $t3, $t4, auto_generated_loop_ret_1
move $a0, $t0
move $a1, $t3
add $sp, $sp, -4
sw $ra, 0($sp)
jal mips_rem
lw $ra, 0($sp)
add $sp, $sp, 4
move $t5, $v0
move $t6, $t5
li $t7, 0
move $t8, $ra
jal refresh_ra
add $ra, $ra, 8
beq $t6, $t7, auto_generated_if_2
move $ra, $t8
add $t3, $t3, 1
j auto_generated_loop_1


auto_generated_loop_ret_1:
jr $ra


auto_generated_if_3:
move $a0, $t0
add $sp, $sp, -4
sw $ra, 0($sp)
jal mips_print_i32
lw $ra, 0($sp)
add $sp, $sp, 4
jr $ra


auto_generated_loop_0:
beq $t0, $t1, auto_generated_loop_ret_0
li $t2, 1
li $t3, 2
move $t4, $t0
move $t9, $ra
jal auto_generated_loop_1
move $ra, $t9
move $s0, $t2
li $s1, 1
move $s2, $ra
jal refresh_ra
add $ra, $ra, 8
beq $s0, $s1, auto_generated_if_3
move $ra, $s2
add $t0, $t0, 1
j auto_generated_loop_0


auto_generated_loop_ret_0:
jr $ra


main:
li $t0, 2
li $t1, -1
move $s3, $ra
jal auto_generated_loop_0
move $ra, $s3
j mips_exit

