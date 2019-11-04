  # labwork.S
  # Written 2015-2017 by F Lundevall
  # Skeleton file for IS1200/IS1500 lab 1.
  # The macros PUSH and POP are in the public domain.
  # Please add your own code at the end of the file.

  #
  # Please keep the two macros PUSH and POP unchanged
  #
  
  
  

.data
.global delay
.global time2string
.text

  
 
.macro	PUSH reg
	addi	$sp,$sp,-4
	sw	\reg,0($sp)
.endm

.macro	POP reg
	lw	\reg,0($sp)
	addi	$sp,$sp,4
.endm

	# ~~ my code starts here ~~ 
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
