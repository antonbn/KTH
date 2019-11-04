/* mipslabwork.c

   This file written 2015 by F Lundevall
   Updated 2017-04-21 by F Lundevall

   This file should be changed by YOU! So you must
   add comment(s) here with your name(s) and date(s):

   This file modified 2017-04-31 by Ture Teknolog 

   For copyright and licensing, see file COPYING */

#include <stdint.h>   /* Declarations of uint_32 and the like */
#include <pic32mx.h>  /* Declarations of system-specific addresses etc */
#include "mipslab.h"  /* Declatations for these labs */

int prime = 1234567;
int mytime = 0x5957;
int timeoutcount = 0;

char textstring[] = "text, more text, and even more text!";

/* Interrupt Service Routine */
void user_isr( void )
{
	IFS(0) &= ~(1 << 8); // set flag to 0
	if (++timeoutcount == 10) {
		timeoutcount = 0;
		time2string( textstring, mytime );
		display_string( 3, textstring );
		display_update();
		tick(&mytime);
	}
}

/* Lab-specific initialization goes here */
void labinit( void )
{
	// initialize timer
	T2CON = 0x70;	// set prescaling to 1:256
	TMR2 = 0;
	PR2 = 31250; // this will take 100 ms
	T2CON |= 1 << 15; // start timer
	IFS(0) &= ~(1 << 8); // set flag to 0
	
	// enable interrupt for timer 2
	IEC(0) |= (1 << 8);
	IPC(2) |= 0x1F; // set priority to highest
	enable_interrupt();
}

/* This function is called repetitively from the main program */
void labwork( void )
{
	prime = nextprime( prime );
	display_string( 0, itoaconv( prime ) );
	display_update();
}
