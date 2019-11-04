/** README
 ** The program will take in a single integer representing the number of integers to sort. Then that
 * many integers are specified after which the program sorts them in descending order.
 ** The program uses selection sort to sort all numbers in order. Before beginning the sort the program
 * also counts and prints out the number of inversions.
 ** The program is used through the command line by typing in an integer representing how many numbers to
 * be sorted, you then write in that many numbers.
 * Author: Anton Bothin
 * Date: 2018-09-14
 */

import java.util.Scanner;

public class Four {

    // Input [1 2 4 3 5 0] -> Output [5 4 3 2 1 0]
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        int size = sc.nextInt();
        Integer[] arr = new Integer[size];
        for (int i = 0; i < size; i++) {
            arr[i] = sc.nextInt();
        }

        System.out.println("\n#inversions: " + nrOfInversions(arr));
        selectionSort(arr);
    }

    private static int nrOfInversions(Comparable[] a) {
        int inversions = 0;

        for (int i = 0; i < a.length; i++) {
            for (int j = i + 1; j < a.length; j++) {
                // if true the two elements are out of order
                if (greater(a[j], a[i])) {
                    System.out.print("([" + i + ", " + a[i] + "], [" + j + ", " + a[j]+ "]) ");
                    inversions++;
                }
            }
        }

        /* Time complexity:
        * Number of comparisons: (N-1)+(N-2)+...+1 = (N*(N-1))/2 => O(N^2)
        */

        return inversions;
    }

    private static void selectionSort(Comparable[] a) {
        int nrOfSwaps = 0;

        for (int i = 0; i < a.length; i++) {
            int max = i;
            // find smallest value
            for (int j = i + 1; j < a.length; j++) {
                if (greater(a[j], a[max])) {
                    max = j;
                }
            }

            /* Time complexity:
             * Number of comparisons: (N-1)+(N-2)+...+1 = (N*(N-1))/2 => O(N^2)
             */

            if (i != max) {
                swap(a, i, max);
                nrOfSwaps++;
            }
            for (Comparable current : a) {
                System.out.print(current + " ");
            }
            System.out.println();
        }

        System.out.print(nrOfSwaps);
    }

    private static boolean greater(Comparable v, Comparable w) {
        return v.compareTo(w) > 0;
    }

    private static void swap(Comparable[] a, int j, int k) {
        Comparable swap = a[j];
        a[j] = a[k];
        a[k] = swap;
    }

}
