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

int mytime = 0x5957;

char textstring[] = "text, more text, and even more text!";

/* Interrupt Service Routine */
void user_isr( void )
{
  return;
}

/* Lab-specific initialization goes here */
void labinit( void )
{
	// set led-lamps (PORTE) as output
	volatile int* trise_clr = (volatile int*) 0xbf886104;
	*trise_clr = 0xff;
	
	
	TRISDSET = 0x7F << 5;
	
	return;
}

volatile int* portE = (volatile int*) 0xbf886110;

/* This function is called repetitively from the main program */
void labwork( void )
{
	int btns = getbtns();
	int sw = getsw();
	if (btns & 4){ // if BTN4 is pressed
		mytime &= ~(0xF << 12);
		mytime |= sw << 12;
	}
	if (btns & 2){ // if BTN3 is pressed
		mytime &= ~(0xF << 8);
		mytime |= sw << 8;
	}
	if (btns & 1){ // if BTN2 is pressed
		mytime &= ~(0xF << 4);
		mytime |= sw << 4;
	}
	
	delay( 1000 );
	time2string( textstring, mytime );
	display_string( 3, textstring );
	display_update();
	tick( &mytime );
	*portE += 1;
	display_image(96, icon);
}
