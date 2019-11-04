/** README
 ** The program takes in a file and replaces each non alphabetical character
 * with a blank space.
 ** The program goes through each character in the file and checks if it is
 * alphabetical or not.
 ** The program is used through the command line. You should pass a filename
 * as argument, it will then create a new file named output.txt.
 * Author: Anton Bothin
 * Date: 2018-09-26
 */

#include <stdio.h>
#include <ctype.h>


int main(int argc, char *argv[]) {
	int x;
	FILE *file = fopen(argv[1], "rb+");
	FILE *output = fopen("output.txt", "wb");
	while ((x = fgetc(file)) != EOF) {
		if(isalpha(x) || x == '\n' || x == '\r') {
			fputc(x, output);
		} else {
			fputc(' ', output);
		}
	}
	
	fclose( file );
	
	return 0;
}