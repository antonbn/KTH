/* logic.c

   This file written 2018 by Carina Wickström
	
	Updated 2018-02-24 by Carina Wickström
	- Created code for collision
	
	Updated 2018-02-28 by Carina Wickström
	- Created the following functions: spawn_player, spawn_opponents, respawn_pixel, 
	pixel_was_eaten
	
	Updated 2018-02-28 by Anton Bothin
	- Implemented the function update_highscore
	
	Updated 2018-02-28 by Carina Wickström
	- Now bigger circles kill smaller circles
	
   For copyright and licensing, see file COPYING */

#include <stdint.h>   /* Declarations of uint_32 and the like */
#include <pic32mx.h>  /* Declarations of system-specific addresses etc */
#include <math.h>
#include "global.h"  /* Declatations for these labs */

void *stdin, *stdout, *stderr;

int calculate_closest_food(Circle c){
	int x_diff;
	int y_diff;
	int distance;
	int distance_min = 10000;
	int closest_food_index;
	
	int i;
	for(i = 0; i < 5; i++){
		x_diff = foods[0][i] - c.x;
		y_diff = foods[1][i] - c.y;
		distance = pythagoras(x_diff, y_diff);
		
		if(distance<distance_min){
			distance_min=distance;
			closest_food_index = i;
		}
	}
	return closest_food_index;
}


void respawn_pixel(int index){
	foods[0][index] = get_rand_x();
	foods[1][index] = get_rand_y();
}

void opponent_eats_pixel(){
	int x_diff;
	int y_diff;
	int distance;
	
	int o;
	int i;
	
	for(o = 0; o < 3; o++){
		for(i = 0; i < 5; i++){
			x_diff = foods[0][i] - opponents[o].x;
			y_diff = foods[1][i] - opponents[o].y;
			distance = pythagoras(x_diff, y_diff);
			
			if(distance<opponents[o].radius){
				opponents[o].radius++;
				respawn_pixel(i);
			}
		}
	}
}

int find_closest_smaller_opponent_index_within_range(Circle c){
	int i;
	int return_index = -1;
	int distance;
	int x_diff;
	int y_diff;
	int min_dis = 10000;
	
	for(i = 0; i < 3; i++){
		x_diff = c.x - opponents[i].x;
		y_diff = c.y - opponents[i].y;
		distance = pythagoras(x_diff, y_diff);
		
		if(c.radius > opponents[i].radius){
			if(distance < min_dis){
				min_dis = distance;
				if(distance < c.radius + 30){
					return_index = i;
				}
			}
		}
	}
	
	return return_index;
}

int is_player_a_target(Circle c){
	int distance;
	int min_dis;
	int x_diff;
	int y_diff;
	int i;
	
	for(i = 0; i < 3; i++){
		x_diff = c.x - player.x;
		y_diff = c.y - player.y;
		distance = pythagoras(x_diff, y_diff);
		
		if (c.radius > player.radius) {
			if (distance < min_dis) {
				min_dis = distance;
				if (distance < c.radius + 30) {
					return 1;
				}
			}
		}
	}
	
	return 0;
}

void move_AI_towards_target(Circle* opponent, const Circle target) {
	if(target.x > (*opponent).x){
		(*opponent).x++;
	}else if(target.x < (*opponent).x){
		(*opponent).x--;
	}
	
	if(target.y > (*opponent).y){
		(*opponent).y++;
	}else if(target.y < (*opponent).y){
		(*opponent).y--;
	}
}

void move_AIs(){
	int closest_food[3];
	int i;
	
	for(i = 0; i < 3; i++){
		int closest_target_index = find_closest_smaller_opponent_index_within_range(opponents[i]);	
		
		if(closest_target_index != -1){ // if smaller opponent is nearby
			move_AI_towards_target(&opponents[i], opponents[closest_target_index]);
		}else if(is_player_a_target(opponents[i])){ //else if player is within range of opponent
			move_AI_towards_target(&opponents[i], player);
		}else{ // else, there is no smaller opponent nearby
			closest_food[i] = calculate_closest_food(opponents[i]);
			
			// Convert food to a circle
			Circle food_target;
			food_target.x = foods[0][closest_food[i]];
			food_target.y = foods[1][closest_food[i]];
		
			move_AI_towards_target(&opponents[i], food_target);
		}
	}
}


int collision(Circle c1, Circle c2){
	
	// Calculate the distance between the two circles middle points
	int x_diff = c2.x - c1.x;
	int y_diff = c2.y - c1.y;
	int distance = pythagoras(x_diff, y_diff);
	
	if((distance < c1.radius || distance < c2.radius) && c1.alive && c2.alive){
		
		if(c1.radius < c2.radius){
			return 1;
		}else if(c2.radius <= c1.radius){
			return 2;
		}
	}
	return 0;
}

void handle_collisions(){
	int collider = 0;
	int collidee = 1;

	while(collider < 2){
		int circle_to_die = collision(opponents[collider],opponents[collidee]);
		
		if(circle_to_die==1){
			opponents[collider].alive = 0;
			opponents[collidee].radius += opponents[collider].radius;
		}else if(circle_to_die==2){
			opponents[collidee].alive = 0;
			opponents[collider].radius += opponents[collidee].radius;
		}
		
		if(collidee++ == 2){
			
			collider++;
			collidee = collider+1;	
		}
	}
	
	collidee = 0;
	while(collidee < 3){
		int circle_to_die = collision(player, opponents[collidee]);
		
		if(circle_to_die == 1){
			player.alive = 0;
		}else if(circle_to_die == 2){
			opponents[collidee].alive = 0;
			player.radius += opponents[collidee].radius;
			current_score += opponents[collidee].radius;
		}
		
		collidee++;
	}
}

void spawn_player(){
	//OBS: icon index starts at 0
	player.x = get_rand_y();
	player.y = get_rand_y();
	
	player.radius = 4;
	player.alive = 1;	
}

void spawn_opponents(){
	int i;
	for(i = 0; i < 3; i++){
		opponents[i].x = get_rand_y() + (i + 1) * 32;
		opponents[i].y = get_rand_y();
		
		opponents[i].radius = 3 + difficulty;
		opponents[i].alive = 1;
	}
}


//update in-game high score, check if a pixel was eaten, respawn pixel at random location
void player_eats_pixel(){
	int x_diff;
	int y_diff;
	int distance;
	
	int i;
	for(i = 0; i < 5; i++){
		x_diff = player.x - foods[0][i];
		y_diff = player.y - foods[1][i];
		distance = pythagoras(x_diff, y_diff);
	
		if(distance < player.radius){
			player.radius++;
			current_score++;
			//randomize new food location
			respawn_pixel(i);
		}
	}	
}

void update_highscore(){
	// highscores will always be sorted in descending order
	int i;
	for (i = 0; i < 3; i++) {
		if (current_score > highscores[i]) {
			int temp_score = highscores[i];
			highscores[i] = current_score;
			// To push down
			current_score = temp_score;
		}
	}
}
