/* game.c

   This file written 2015 by F Lundevall
   Updated 2017-04-21 by F Lundevall

   This file should be changed by YOU! So you must
   add comment(s) here with your name(s) and date(s):

	Updated 2018-02-14 by Anton Bothin
	- Removed code parts not necessary for the project
	
	Updated 2018-02-24 by Carina Wickström
	- Code for buttons
	
	Updated 2018-02-25 by Anton Bothin
	- Added code for timer and interrupt
	
	Updated 2018-02-28 by Carina Wickström
	- Added bits of food that can be eaten and respawned, 
	
	Updated 2018-02-28 by Anton Bothin
	- Player can warp from one side to another
	- Local high score

   For copyright and licensing, see file COPYING */

#include <stdlib.h>		/*This is for using random ints*/
#include <stdint.h>   /* Declarations of uint_32 and the like */
#include <pic32mx.h>  /* Declarations of system-specific addresses etc */
#include "global.h"  /* Declatations for these labs */


#define FPS 10
#define PLAYER_SPEED 4

/* Global variables */
Circle player;
Circle opponents[];

// used because current_score is changed when game is over
char* score_string;
int current_score = 0;

// to not go directly back to the main menu once game is over
int btn_pressed;

int foods[2][5];

int any_opponent_alive() {
	int i;
	for (i = 0; i < 3; i++) {
		if (opponents[i].alive) {
			return 1;
		}
	}
	
	return 0;
}

/* Interrupt Service Routine */
void user_isr( void ){
	IFS(0) &= ~(1 << 8); // set flag to 0
	
	if(player.alive && any_opponent_alive()){
		if(!in_menu){
			game_loop();
		}
	} else {
		if (btn_pressed == -1) {
			btn_pressed = get_buttons() & 0xF;
		}
		if (!score_string){
			score_string = itoaconv(current_score);
		}
		
		display_string(3, score_string);
		if (player.alive) { // You won, all opponents are dead
			display_image_and_string(win);
		} else { // You lost, you're dead
			display_image_and_string(lose);
		}
		
		if ((get_buttons() & 0xF) && !btn_pressed) {
			player.alive = 0;
			in_menu = 1;
			// disable interrupts from timer 2
			IECCLR(0) = (1 << 8);
		} else if (!(get_buttons() & 0xF)) {
			btn_pressed = 0;
		}
		
		// Must run after score is shown, since it changes the value of current_score
		update_highscore();
		current_score = 0;
	}
}

void io_setup(){
	// set buttons and switches (PORTD) as input
	TRISDSET = 0x7F << 5;
	// set button 1 (PORTF) as input
	TRISFSET = 0x2;
}

void timer_setup(){
	// initialize timer
	IFS(0) &= ~(1 << 8); // set flag to 0
	T2CON = 0x70;	// set prescaling to 1:256
	TMR2 = 0;
	PR2 = (80000000 / 256) / FPS; // timer will interrupt FPS times per second
	T2CONSET = 1 << 15; // start timer
}

void interrupt_setup(){
	// enable interrupt for timer 2
	IEC(0) |= (1 << 8);
	IPC(2) |= 0x1F; // set priority to highest
	asm ("ei");	// enable interrupts
}

void game_init(){
	io_setup();
	timer_setup();
	interrupt_setup();
	
	score_string = 0;
	btn_pressed = -1;
	
	spawn_player();
	spawn_opponents();
	
	int i;
	for(i=0;i < 5;i++){
		foods[0][i] = get_rand_x();
		foods[1][i] = get_rand_y();
	}
}

void game_loop() { // the actual game, look here
	clear_screen();
	
	move_player();
	move_AIs();
	
	draw_food();
	draw_filled_circle(player.x, player.y, player.radius);
	
	int i;
	for(i = 0; i < 3; i++){
		if(opponents[i].alive){
			draw_circle(opponents[i].x, opponents[i].y, opponents[i].radius);
		}
	}
	
	player_eats_pixel();
	opponent_eats_pixel();
	handle_collisions();

	display_screen();
}

void move_player(){
	int btns = get_buttons();
			
	switch (btns) {
		case 2:
			player.x += PLAYER_SPEED;
			break;
		case 4:
			player.x -= PLAYER_SPEED;
			break;
		case 1:
			player.y -= PLAYER_SPEED;
			break;
		case 8:
			player.y += PLAYER_SPEED;
			break;
	}
	
	player.x = loop_within_interval(player.x, 0, 127);
	player.y = loop_within_interval(player.y, 0, 31);
}
