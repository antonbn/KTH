/** README
* The program will take in a string as input and output it in reverse.
* The implementation uses a loop to constantly check if a character is 
** inputed. It will store this character in an array and read the next 
** character until a newline is read. The program will then output this
** array in reverse order.
* The program is used through the command line by typing in a string and 
** then pressing enter.
* Author: Anton Bothin
* Date: 2018-09-07
*/

#include <stdio.h>

int main() {
	char arr[100];
	int length = 0;
	int c;
	// loop until a newline is inputed
	while (((c = getchar()) != '\n')) {
		arr[length] = c;
		length += 1;
	}
	// output the char array in reverse order
	for (int i = length - 1; i >= 0; i -= 1) {
		putchar(arr[i]);
	}
	
	return 0;
}