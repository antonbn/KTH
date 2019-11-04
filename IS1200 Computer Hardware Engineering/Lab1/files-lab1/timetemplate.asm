  # timetemplate.asm
  # Written 2015 by F Lundevall
  # Copyright abandonded - this file is in the public domain.

.macro	PUSH (%reg)
	addi	$sp,$sp,-4
	sw	%reg,0($sp)
.end_macro

.macro	POP (%reg)
	lw	%reg,0($sp)
	addi	$sp,$sp,4
.end_macro

	.data
	.align 2
mytime:	.word 0x5957
timstr:	.ascii "text more text lots of text\0"
	.text
main:
	# print timstr
	la	$a0,timstr
	li	$v0,4
	syscall
	nop
	# wait a little
	li	$a0,1000
	jal	delay
	nop
	# call tick
	la	$a0,mytime
	jal	tick
	nop
	# call your function time2string
	la	$a0,timstr
	la	$t0,mytime
	lw	$a1,0($t0)
	jal	time2string
	nop
	# print a newline
	li	$a0,10
	li	$v0,11
	syscall
	nop
	# go back and do it all again
	j	main
	nop
# tick: update time pointed to by $a0
tick:	lw	$t0,0($a0)	# get time
	addiu	$t0,$t0,1	# increase
	andi	$t1,$t0,0xf	# check lowest digit
	sltiu	$t2,$t1,0xa	# if digit < a, okay
	bnez	$t2,tiend
	nop
	addiu	$t0,$t0,0x6	# adjust lowest digit
	andi	$t1,$t0,0xf0	# check next digit
	sltiu	$t2,$t1,0x60	# if digit < 6, okay
	bnez	$t2,tiend
	nop
	addiu	$t0,$t0,0xa0	# adjust digit
	andi	$t1,$t0,0xf00	# check minute digit
	sltiu	$t2,$t1,0xa00	# if digit < a, okay
	bnez	$t2,tiend
	nop
	addiu	$t0,$t0,0x600	# adjust digit
	andi	$t1,$t0,0xf000	# check last digit
	sltiu	$t2,$t1,0x6000	# if digit < 6, okay
	bnez	$t2,tiend
	nop
	addiu	$t0,$t0,0xa000	# adjust last digit
tiend:	sw	$t0,0($a0)	# save updated result
	jr	$ra		# return
	nop

  # you can write your code for subroutine "hexasc" below this line
  #



hexasc: 

	addi $v0, $0, 0
	
	andi $a0, $a0, 0xF
	
	ble $a0, 9, returnNumber
	nop
	ble $a0, 15, returnLetter
	nop
	
	jr $ra
	nop
	
returnNumber: 

	addi $v0, $a0, 0x30 
	jr $ra
	nop
	  
returnLetter: 
	
	addi $v0, $a0, 0x37
	jr $ra
	nop

delay:
  	
 	# a0 = int ms (parameter value)
 	# a0 = 2, när delay anropas
 	
 	addi $t0, $0, 13460 # the amount of clockcycles on  1 millisec, changeable
 	add $t1, $0, $t0 # sätt t1 till t0 = 350
 	
 outerDelayLoop:
 
 	ble $a0, $0, delayDone # while( ms > 0 ) = ms amount of times
 	nop
 	sub $a0, $a0, 1 # a0--
 	add $t1, $0, $t0 # sätt t1 till t0 = 350
 	
 innerDelayLoop:
 	
 	ble $t1, $0, outerDelayLoop # 350 times:
 	nop
 	sub $t1, $t1, 1
 	j innerDelayLoop
 	nop
 	
delayDone:	
 	
 	jr $ra
 	nop
 	
time2string: 

	PUSH $ra
	PUSH $s3
	PUSH $s4
	
	addi $s4,$a0,0
	addi $s3,$a1,0
	
	move $a0, $a1
	jal hexasc
	nop
	sb $v0, 4($s4)
	
	srl $a0, $s3, 4
	jal hexasc
	nop
	sb $v0, 3($s4)
	
	#X at every minute
	lb $t0, 4($s4)
	lb $t1, 3($s4)
	
	or $t0, $t0, $t1
	addi $t1, $0, 0x30
	bne $t0, $t1, noX
	nop
	addi $t0, $0, 0x58
	sb $t0, 5($s4)
	sb $0, 6($s4)
	j xHappend
	nop
	noX:
	sb $0, 5($s4)
	xHappend:
	
	#kolon
	addi $t0, $0, 0x3A # kolon // 8 bit size
	sb $t0, 2($s4)
	
	srl $a0, $s3, 8
	jal hexasc
	nop
	sb $v0, 1($s4)
	
	srl $a0, $s3, 12
	jal hexasc
	nop
	sb $v0, 0($s4)
	
	
	
	POP $s4
	POP $s3
	POP $ra
	jr $ra
	nop
