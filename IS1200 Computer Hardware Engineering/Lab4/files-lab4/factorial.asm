
.text

	addi $a0,$0,8     
	addi $v0,$0,1     
loop:	beq  $a0,$0,done  # test, jump to done. 
        addi $a0,$a0,-1   # decrement
	addi $a1,$a0,0	  # set $a1 to $a0
	addi $a2,$v0,0   # set $a2 to $v0
inner:	beq  $a1,$0,loop  # test, jump to loop.
        addi $a1,$a1,-1   # decrement
	add  $v0,$v0,$a2
        beq  $0,$0,inner
done:	beq  $0,$0,done
