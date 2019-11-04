  # labwork.S
  # Written 2015-2017 by F Lundevall
  # Skeleton file for IS1200/IS1500 lab 1.
  # The macros PUSH and POP are in the public domain.
  # Please add your own code at the end of the file.

  #
  # Please keep the two macros PUSH and POP unchanged
  #
  
  
 

.data
.global enable_interrupt
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
enable_interrupt:
	ei
	jr $ra
	nop
	
hexasc: 
	andi $a0, $a0, 0xF
	ble $a0, 9, returnNumber
	
	addi $v0, $a0, 0x37
	jr $ra
	nop
	
	returnNumber: 
	addi $v0, $a0, 0x30 
	jr $ra
	nop

delay:
 	addi $t0, $0, 13460 # the amount of clockcycles on  1 millisec, changeable
	outerDelayLoop:
 	ble $a0, $0, delayDone # while( ms > 0 ) = ms amount of times
 	nop
 	sub $a0, $a0, 1 # a0--
 	add $t1, $0, $t0
 	
	innerDelayLoop:
 	ble $t1, $0, outerDelayLoop # $t0 times:
 	nop
 	sub $t1, $t1, 1
 	j innerDelayLoop
	nop
 	
	delayDone:	
 	jr $ra
 	nop
 	
time2string: 
	PUSH $ra
	PUSH $s0
	PUSH $s1
	
	addi $s0, $a0, 0
	addi $s1, $a1, 0
	
	sb $0, 5($s0)	# end-of-string-marker
	
	move $a0, $a1
	jal hexasc
	nop
	sb $v0, 4($s0)
	
	srl $a0, $s1, 4
	jal hexasc
	nop
	sb $v0, 3($s0)
	
	#kolon
	addi $t0, $0, 0x3A # kolon // 8 bit size
	sb $t0, 2($s0)
	
	srl $a0, $s1, 8
	jal hexasc
	nop
	sb $v0, 1($s0)
	
	srl $a0, $s1, 12
	jal hexasc
	nop
	sb $v0, 0($s0)
	
	
	
	POP $s1
	POP $s0
	POP $ra
	
	jr $ra
	nop
