/** README
* The program will take in a string as input and output it in reverse.
* The implementation uses recursion to get the input. Each character is outputted after the method has been called resulting in the entire string being outputted in reverse order.
* The program is used through the command line by typing in a string and 
** then pressing enter.
* Author: Anton Bothin
* Date: 2018-09-07
*/

#include <stdio.h>

int main() {
	int c;
	if (((c = getchar()) == '\n')) {
		return 0;
	}
	
	main();
	putchar(c);
	
	return 0;
}