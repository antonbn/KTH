/* global.h
   Header file for all labs.
   This file written 2015 by F Lundevall
   Some parts are original code written by Axel Isaksson
	
	Updated 2018-02-25 by Anton Bothin
	- Added a struct for circles
	
	Updated 2018-02-28 by Carina wickström
	- Circles are now alive or dead, how exciting!

   For copyright and licensing, see file COPYING */

typedef struct Circle {
	int x;
	int y;
	int radius;
	int alive;
}Circle;
   
/* Declare display-related functions from mipslabfunc.c */
void display_image_and_string(const uint8_t *data);
void display_image(const uint8_t *data);
void display_init(void);
void display_string(int line, char *s);
void display_update(int);
uint8_t spi_send_recv(uint8_t data);

/* Declare lab-related functions from mipslabfunc.c */
char * itoaconv( int num );
void update(void);
void quicksleep(int cyc);

/* Declare display_debug - a function to help debugging.

   After calling display_debug,
   the two middle lines of the display show
   an address and its current contents.

   There's one parameter: the address to read and display.

   Note: When you use this function, you should comment out any
   repeated calls to display_image; display_image overwrites
   about half of the digits shown by display_debug.
*/
void display_debug( volatile int * const addr );

void clear_screen();
void draw_circle(int px, int py, int radius);
void draw_food();
void draw_pixel(int x, int y);
void display_screen();

/* Declare bitmap array containing font */
extern const uint8_t const font[128*8];
/* Declare bitmap array containing logo, win and lose screen */
extern const uint8_t const logo[512];
extern const uint8_t const win[512];
extern const uint8_t const lose[512];
/* Declare text buffer for display output */
extern char textbuffer[4][16];

/* Declare functions written by students.
   Note: Since we declare these functions here,
   students must define their functions with the exact types
   specified in the laboratory instructions. */
   
/* helpers.c */
extern int time_until_game_start;
extern int num_of_btn_presses;

int *fixed_to_string(int num);
int loop_within_interval(int n, int min, int max);
int get_buttons(void);
int get_switches(void);
int pythagoras(int x, int y);

/* game.c */
void game_init();
void game_loop();
void move_player();
extern int foods[2][5];
extern int current_score;
extern Circle player;
extern Circle opponents[3];

/* logic.c */
void spawn_player();
void spawn_opponents();
void pixel_was_eaten(Circle c, int foods[][5]);
void update_highscore();
void move_AIs();

/* menu.c */
extern int highscores[3];
extern int nightmode;
extern int in_menu;
extern int difficulty;
void show_main_menu();

/* logic */
void handle_collisions();