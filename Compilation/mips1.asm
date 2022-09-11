​	.text
# code assembleur
    li  $t0, 1
    li $t1, 2
    add $t2, $t0, $t1
    lw  $t0, 4($t2)
    sw  $t0, 8($t2)

    lw  $t0,    x
    lw  $t1,    y

    li $t2, 5
    addi $sp, $sp, 4 #pop
    lw $t3


    .data
x:  .word 0xff
y:  .word 0xf