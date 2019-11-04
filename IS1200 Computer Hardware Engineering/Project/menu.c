/* menu.c

	This file written 2018 by Carina Wickström
	
	Updated 2018-02-24 by Carina Wickström
	- Made navigation possible in menu
	
	Updated 2018-02-25 by Anton Bothin
	- Added a start animation for when the chipkit is first turned on
	
	Updated 2018-02-26 by Anton Bothin
	- Added support for selecting different menu options (only "start game" does something)
	
	Updated 2018-02-27 by Carina Wickström
	- Created sub-menu options now everything works 
	
	Updated 2018-02-24 by Carina Wickström
	- Added day and night mode!
	
   For copyright and licensing, see file COPYING */

#include <stdint.h>   /* Declarations of uint_32 and the like */
#include <pic32mx.h>  /* Declarations of system-specific addresses etc */
#include "global.h"  /* Declatations for these labs */

// Global variables
int highscores[] = {0, 0, 0};
int time_until_game_start = 0;
int num_of_btn_presses = 0;
int in_menu = 1;
int row = 0;
int nightmode = 0;
int difficulty = 0;

typedef enum menuOption menuOption;
enum menuOption {mainMenu, credits, highscore, settings};

menuOption menuOp = mainMenu;
//if ( menuOp == mainMenu );


void show_settings(){
	if (!nightmode) {
		display_string(0, "night mode");
	} else {
		display_string(0, "day mode");
	}
	display_string(1, "easy");
	display_string(2, "medium");
	display_string(3, "hard");
	
	display_update(row);
}

void show_highscore(){
	int i;
	
	display_string(0,"Highscores: ");
	for (i = 1; i < 4; i++) {
		display_string(i, itoaconv(highscores[i - 1]));
	}
	display_update(-1);
	
}

void show_credits(){
	display_string(0,"Game makers:");
	display_string(1,"Carina Wickström");
	display_string(2,"Anton Bothin");
	display_string(3,"");
	display_update(-1);
}

void show_main_menu() {
	display_string(0,"start game");
	display_string(1,"settings");
	display_string(2,"high scores");
	display_string(3,"credits");
	display_update(row);
}

void select_main_menu_option() {
	switch(row) {
		case 0:	// start game
			in_menu = 0;
			game_init();
			break;
		case 1: // settings
			menuOp = settings;
			break;
		case 2: // high scores
			menuOp = highscore;
			break;
		case 3:	// credits
			menuOp = credits;
			break;
	}
}

void select_settings_menu_option(){
	switch(row) {
		case 0:	// switch mode
			nightmode = !nightmode;
			break;
		case 1: // easy mode
			difficulty = 0;
			break;
		case 2: // medium mode
			difficulty = 1;
			break;
		case 3: // hard mode
			difficulty = 2;
			break;
	}
	
}
   
void navigate_menu(){
	static int btnPressed = 0;
	
	int btns = get_buttons();
	
	switch (btns) {
		case 0:	
			btnPressed = 0;
			break;
		// button 2 pressed, select option
		case 1:
			if (!btnPressed) {
				//if in submenu
				if(menuOp == credits || menuOp == highscore){
					//go back to main 
					menuOp = mainMenu;
				}else if(menuOp == settings){
					select_settings_menu_option();
					//go back to main 
					menuOp = mainMenu;
					
				}else{
					select_main_menu_option();
				}
			}
			btnPressed = 1;
			num_of_btn_presses++;
			break;
		// button 3 pressed, go down
		case 2:
			if (!btnPressed) {
				row++;
			}
			btnPressed = 1;
			num_of_btn_presses++;
			break;
		// button 4 pressed, go up
		case 4:
			if (!btnPressed) {
				row--;
			}
			btnPressed = 1;
			num_of_btn_presses++;
			break;
	}
	
	if (row < 0) {
		row = 3;
	} else if (row > 3) {
		row = 0;
	}
}

void start_animation() {
	uint8_t animation[512];
	
	int i;
	for (i = 0; i < 512; i++){
		animation[i] = 255;
	}
	
	int x = 0;
	int y = 31;
	while (x != 127 || y != 0){		
		display_image(animation);
		
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
		int value = 1 << y % 8;
		
		// If this pixel is black in the logo, make it black in the animation (slowly reveal the text)
		if (!(logo[pos] & value)) {
			animation[pos] -= value;
		}
		
		// Increment x and y in a diagonal fashion
		x++;
		if (--y < 0){
			y = 31;
			x -= 31;
		}
	}	
}


void show_correct_menu(){
	if(menuOp == mainMenu){
		show_main_menu();
	}else if(menuOp == highscore){
		show_highscore();
	}else if(menuOp == credits){
		show_credits();
	}else if(menuOp == settings){
		show_settings();
	}
	
	navigate_menu();
}


