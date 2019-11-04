/** README
 ** The program has a premade array containing integers (both positive
 * and negative). It then seperates these so that the negative items
 * are first in the list.
 ** The program loops until it fins a negative value while keeping track
 * of the index that the first positive value resides at. When finding 
 * a negative value these values are swapped.
 ** The program is used through the command line, it will straight away
 * show a list with negative values first and positive values after that.
 * Author: Anton Bothin
 * Date: 2018-09-14
 */

#include <stdio.h>

void swap(int arr[], int j, int k) {
	int temp = arr[j];
	arr[j] = arr[k];
	arr[k] = temp;
}

int main() {
	int length = 8;
	int arr[] = {1, 2, -2, 3, -4, -1, 6, -1};
	
	int counter = 0;
	for (int i = 0; i < length; i += 1) {
		if (arr[i] < 0) {
			swap(arr, counter, i);
			counter++;
		}
	}
	
	/* Time complexity:
    * Number of comparisons: N => O(N)
    */
		
	for (int i = 0; i < length; i += 1) {
		printf("%d ", arr[i]);
	}
	
	return 0;
}