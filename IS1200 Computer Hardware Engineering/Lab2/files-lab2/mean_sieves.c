#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define COLUMNS 6

int nOfTimesPrinted = 0;

void print_number(int n){
	printf("%10d ", n);
	if (++nOfTimesPrinted == COLUMNS){
		nOfTimesPrinted = 0;
		printf("\n");
	}
}

void mean_sieves(int n){
	char arr[n - 1];	// Do not care about number 0 and 1
	for (int i = 0; i < n - 1; i++){
		arr[i] = 1;
	}
	
	for(int i = 2; i <= sqrt(n); i++){
		if (arr[i - 2]) {
			for (int j = i * i - 2; j < n - 1; j += i) {
				arr[j] = 0;
			}
		}
	}
	
	int sum = 0;
	int numOfPrimes = 0;
	for (int i = 0; i < n - 1; i++){
		if (arr[i]){
			sum += (i + 2);
			numOfPrimes += 1;
		}
	}
	
	double average = sum / (double) numOfPrimes;
	
	printf("%f\n", average);
}

// 'argc' contains the number of program arguments, and
// 'argv' is an array of char pointers, where each
// char pointer points to a null-terminated string.
int main(int argc, char *argv[]){
  if(argc == 2)
    mean_sieves(atoi(argv[1]));
  else
    printf("Please state an interger number.\n");
  return 0;
}

 
