/* helpers.c
   This file written 2018 by Anton Bothin
   Some parts are original code written by F Lundevall, Axel Isaksson

   Updated 2018-02-14 by Anton Bothin
	- Moved itoaconv from display.c to here
	- Created functions get_switches and get_buttons
	
	Updated 2018-02-24 by Carina Wickström
	- Math functions needed for the game logic
	
	Updated 2018-02-25 by Anton Bothin
	- Created function loop_within_interval
	
	Updated 2018-02-28 by Anton Bothin
	- Updated get_buttons to also return button 1 (from PORTF)
	- Seed for random value
	
	Updated 2018-02-28 by Carina Wickström
	- Created functions for randomly generating x and y values
   
   For copyright and licensing, see file COPYING */

#include <stdint.h>   /* Declarations of uint_32 and the like */
#include <stdlib.h>	  /* Random numbers */
#include <pic32mx.h>  /* Declarations of system-specific addresses etc */
#include <math.h>
#include "global.h"  /* Declatations for these labs */

void *stdin, *stdout, *stderr, *errno;

int get_rand_x() {
   return (rand() % 128);
}

int get_rand_y () {
   return (rand() % 32);
}

/* Will be run first thing when the "start game" option is selected */
void set_seed_for_rand(){
	int seed = time_until_game_start ^ num_of_btn_presses;
	srand(seed);
}

int loop_within_interval(int n, int min, int max) {
	if (n < min){
		n = max;
	} else if (n > max){
		n = min;
	}
	
	return n;
}

int pythagoras(int x, int y) {
    return sqrt(x * x + y * y);
}

int get_switches(void) {
	return (PORTD >> 8) & 0xF;
}

int get_buttons(void) {
	int portd_btns = (PORTD >> 5) & 7;
	int portf_btn = PORTF & 2;
	
	// Button 1 on msb because we're lazy and don't want to change any code
	return (portf_btn << 2) | portd_btns;
}

/*
 * itoa
 * 
 * Simple conversion routine
 * Converts binary to decimal numbers
 * Returns pointer to (static) char array
 * 
 * The integer argument is converted to a string
 * of digits representing the integer in decimal format.
 * The integer is considered signed, and a minus-sign
 * precedes the string of digits if the number is
 * negative.
 * 
 * This routine will return a varying number of digits, from
 * one digit (for integers in the range 0 through 9) and up to
 * 10 digits and a leading minus-sign (for the largest negative
 * 32-bit integers).
 * 
 * If the integer has the special value
 * 100000...0 (that's 31 zeros), the number cannot be
 * negated. We check for this, and treat this as a special case.
 * If the integer has any other value, the sign is saved separately.
 * 
 * If the integer is negative, it is then converted to
 * its positive counterpart. We then use the positive
 * absolute value for conversion.
 * 
 * Conversion produces the least-significant digits first,
 * which is the reverse of the order in which we wish to
 * print the digits. We therefore store all digits in a buffer,
 * in ASCII form.
 * 
 * To avoid a separate step for reversing the contents of the buffer,
 * the buffer is initialized with an end-of-string marker at the
 * very end of the buffer. The digits produced by conversion are then
 * stored right-to-left in the buffer: starting with the position
 * immediately before the end-of-string marker and proceeding towards
 * the beginning of the buffer.
 * 
 * For this to work, the buffer size must of course be big enough
 * to hold the decimal representation of the largest possible integer,
 * and the minus sign, and the trailing end-of-string marker.
 * The value 24 for ITOA_BUFSIZ was selected to allow conversion of
 * 64-bit quantities; however, the size of an int on your current compiler
 * may not allow this straight away.
 */
#define ITOA_BUFSIZ ( 24 )
char * itoaconv( int num )
{
  register int i, sign;
  static char itoa_buffer[ ITOA_BUFSIZ ];
  static const char maxneg[] = "-2147483648";
  
  itoa_buffer[ ITOA_BUFSIZ - 1 ] = 0;   /* Insert the end-of-string marker. */
  sign = num;                           /* Save sign. */
  if( num < 0 && num - 1 > 0 )          /* Check for most negative integer */
  {
    for( i = 0; i < sizeof( maxneg ); i += 1 )
    itoa_buffer[ i + 1 ] = maxneg[ i ];
    i = 0;
  }
  else
  {
    if( num < 0 ) num = -num;           /* Make number positive. */
    i = ITOA_BUFSIZ - 2;                /* Location for first ASCII digit. */
    do {
      itoa_buffer[ i ] = num % 10 + '0';/* Insert next digit. */
      num = num / 10;                   /* Remove digit from number. */
      i -= 1;                           /* Move index to next empty position. */
    } while( num > 0 );
    if( sign < 0 )
    {
      itoa_buffer[ i ] = '-';
      i -= 1;
    }
  }
  /* Since the loop always sets the index i to the next empty position,
   * we must add 1 in order to return a pointer to the first occupied position. */
  return( &itoa_buffer[ i + 1 ] );
}