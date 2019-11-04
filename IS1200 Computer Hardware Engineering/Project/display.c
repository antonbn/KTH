/* display.c
	This file written 2015 by F Lundevall
	Some parts are original code written by Axel Isaksson

	Updated 2018-02-14 by Anton Bothin
	- Created the function display_pixel
	
	Updated 2018-02-23 by Anton Bothin
	- Changed display_pixel to draw_pixel (draws to a vector instead of directly to the display)
	
	Updated 2018-02-23 by Carina Wickström
	- Wrote functions for displaying graphics (clear_screen, draw_circle)
	
	Updated 2018-02-25 by Anton Bothin
	- Changed bitmap to cover entire screen (32x128), not just 32x32
	- Created function display_screen
	- Changed circle-functions to be based on top left corner instead of center (this makes it easier to draw on the display)
   
	For copyright and licensing, see file COPYING */

#include <stdint.h>   /* Declarations of uint_32 and the like */
#include <pic32mx.h>  /* Declarations of system-specific addresses etc */
#include "global.h"  /* Declatations for these labs */
#define SCREEN_SIZE 512
/* Declare bitmap array containing screen */
static uint8_t screen[SCREEN_SIZE];

/* Declare a helper function which is local to this file */
static void num32asc( char * s, int ); 

#define DISPLAY_CHANGE_TO_COMMAND_MODE (PORTFCLR = 0x10)
#define DISPLAY_CHANGE_TO_DATA_MODE (PORTFSET = 0x10)

#define DISPLAY_ACTIVATE_RESET (PORTGCLR = 0x200)
#define DISPLAY_DO_NOT_RESET (PORTGSET = 0x200)

#define DISPLAY_ACTIVATE_VDD (PORTFCLR = 0x40)
#define DISPLAY_ACTIVATE_VBAT (PORTFCLR = 0x20)

#define DISPLAY_TURN_OFF_VDD (PORTFSET = 0x40)
#define DISPLAY_TURN_OFF_VBAT (PORTFSET = 0x20)

/* quicksleep:
   A simple function to create a small delay.
   Very inefficient use of computing resources,
   but very handy in some special cases. */
void quicksleep(int cyc) {
	int i;
	for(i = cyc; i > 0; i--);
}

/* display_debug
   A function to help debugging.

   After calling display_debug,
   the two middle lines of the display show
   an address and its current contents.

   There's one parameter: the address to read and display.

   Note: When you use this function, you should comment out any
   repeated calls to display_image; display_image overwrites
   about half of the digits shown by display_debug.
*/   
void display_debug( volatile int * const addr )
{
  display_string( 1, "Addr" );
  display_string( 2, "Data" );
  num32asc( &textbuffer[1][6], (int) addr );
  num32asc( &textbuffer[2][6], *addr );
  display_update(-1);
}

uint8_t spi_send_recv(uint8_t data) {
	while(!(SPI2STAT & 0x08));
	SPI2BUF = data; // 
	while(!(SPI2STAT & 1));
	return SPI2BUF;
}

void display_init(void) {
	DISPLAY_CHANGE_TO_COMMAND_MODE;
	
	quicksleep(10);
	DISPLAY_ACTIVATE_VDD;
	quicksleep(1000000);
	
	spi_send_recv(0xAE);
	DISPLAY_ACTIVATE_RESET;
	quicksleep(10);
	DISPLAY_DO_NOT_RESET;
	quicksleep(10);
	
	spi_send_recv(0x8D);
	spi_send_recv(0x14);
	
	spi_send_recv(0xD9);
	spi_send_recv(0xF1);
	
	DISPLAY_ACTIVATE_VBAT;
	quicksleep(10000000);
	
	spi_send_recv(0xA1);
	spi_send_recv(0xC8);
	
	spi_send_recv(0xDA);
	spi_send_recv(0x20);
	
	spi_send_recv(0xAF);
}

void clear_screen(){
	int i;
	for (i = 0; i < SCREEN_SIZE; i++){
		screen[i] = 0;
	}
}

void draw_food(){
	int i;
	for(i=0;i < 5;i++){
		draw_pixel(foods[0][i],foods[1][i]);
	}
	
}

void draw_filled_circle(int px, int py, int radius){
	int y;
	int x;
    for (y=-radius; y <= radius; y++)  {
        for (x=-radius; x <= radius; x++){
			// If (x, y) is within the circle
            if (pythagoras(x, y) <= radius){
				//x and y are offsets from px and py
				draw_pixel(px + x, py + y);
			}
		}
    }
}

void draw_circle(int px, int py, int radius){
	int y;
	int x;
    for (y=-radius; y <= radius; y++)  {
        for (x=-radius; x <= radius; x++){
			// If (x, y) is within the circle
            if (pythagoras(x, y) == radius){
				//x and y are offsets from px and py
				draw_pixel(px + x, py + y);
			}
		}
    }
}

void draw_pixel(int x, int y){
	// if (x, y) is outside the screen, don't draw
	if (x < 0 || x > 127)
		return;
	if (y < 0 || y > 31)
		return;
	
	/*
		The screen is split up into 8x8 segments. 
		There are 16 of the segments aligned horizontally and
		4 aligned vertically (the screen is a total of 128x32 pixels).
		
		To get the pos we divide y by 8 (which segment we're on vertically) and
		multiply this value by 128 (the amount of pixels horizontally) to get to
		the correct vertical segment. We then add x to this value to get to the 
		correct pixel in the correct horizontal segment.
		
		To get the value we take y modulo 8 to get the vertical pixel relative to 
		the segment we're on. We then raise 2 with this value to get an 8 bit number where
		only the correct bit is 1.
	*/
	int pos = (y / 8) * 128 + x;
	int value = 1 << (y % 8);
	
	screen[pos] += value;	
}

void display_string(int line, char *s) {
	int i;
	if(line < 0 || line >= 4)
		return;
	if(!s)
		return;
	
	for(i = 0; i < 16; i++)
		if(*s) {
			textbuffer[line][i] = *s;
			s++;
		} else
			textbuffer[line][i] = ' ';
}

void display_screen() {
	display_image(screen);
}

void display_image(const uint8_t *data) {
	int i, j;
	
	for(i = 0; i < 4; i++) {
		DISPLAY_CHANGE_TO_COMMAND_MODE;

		spi_send_recv(0x22);
		spi_send_recv(i);
		
		spi_send_recv(0x0);
		spi_send_recv(0x10);
		
		DISPLAY_CHANGE_TO_DATA_MODE;
		
		if(!nightmode){
			for(j = 0; j < 128; j++){
				spi_send_recv(data[i*128 + j]);
			}
		}else{
			for(j = 0; j < 128; j++){
				spi_send_recv(~data[i*128 + j]);
			}
		}
	}
}

void display_update(int row) {
	int i, j, k;
	int c;
	for(i = 0; i < 4; i++) {
		DISPLAY_CHANGE_TO_COMMAND_MODE;
		spi_send_recv(0x22);
		spi_send_recv(i);
		
		spi_send_recv(0x0);
		spi_send_recv(0x10);
		
		DISPLAY_CHANGE_TO_DATA_MODE;
		
		for(j = 0; j < 16; j++) {
			c = textbuffer[i][j];
			if(c & 0x80)
				continue;
			
			if(!nightmode){
				for(k = 0; k < 8; k++){
					if (i == row){
						spi_send_recv(~font[c*8 + k]);
					}
					else{
						spi_send_recv(font[c*8 + k]);
					}
				}
			}else{
				for(k = 0; k < 8; k++){
					if (i == row){
						spi_send_recv(font[c*8 + k]);
					}
					else{
						spi_send_recv(~font[c*8 + k]);
					}
				}
				
			}
		}
	}
}

void display_image_and_string(const uint8_t *data) {
	int i, j, k;
	int c;
	
	for(i = 0; i < 3; i++) {
		DISPLAY_CHANGE_TO_COMMAND_MODE;

		spi_send_recv(0x22);
		spi_send_recv(i);
		
		spi_send_recv(0x0);
		spi_send_recv(0x10);
		
		DISPLAY_CHANGE_TO_DATA_MODE;
		
		if(!nightmode){
			for(j = 0; j < 128; j++){
				spi_send_recv(data[i*128 + j]);
			}
		}else{
			for(j = 0; j < 128; j++){
				spi_send_recv(~data[i*128 + j]);
			}
		}
	}
	
	// Only display text at the bottom of the screen (i = 3)
	DISPLAY_CHANGE_TO_COMMAND_MODE;

	spi_send_recv(0x22);
	spi_send_recv(3);
	
	spi_send_recv(0x0);
	spi_send_recv(0x10);
	
	DISPLAY_CHANGE_TO_DATA_MODE;
	
	for(j = 0; j < 16; j++) {
		c = textbuffer[3][j];
		if(c & 0x80)
			continue;
		
		for(k = 0; k < 8; k++){
			if(!nightmode){
				spi_send_recv(font[c*8 + k]);
			}else {
				spi_send_recv(~font[c*8 + k]);
			}
		}
	}
}

/* Helper function, local to this file.
   Converts a number to hexadecimal ASCII digits. */
static void num32asc( char * s, int n ) 
{
  int i;
  for( i = 28; i >= 0; i -= 4 )
    *s++ = "0123456789ABCDEF"[ (n >> i) & 15 ];
}
