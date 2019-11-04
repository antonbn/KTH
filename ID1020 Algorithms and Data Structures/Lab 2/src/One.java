/** README
 ** The program will take in a single integer representing the number of integers to sort. Then that
 * many integers are specified after which the program sorts them in ascending order.
 ** The program uses selection sort to sort all numbers in order.
 ** The program is used through the command line by typing in an integer representing how many numbers to
 * be sorted, you then write in that many numbers.
 * Author: Anton Bothin
 * Date: 2018-09-14
 */

import java.util.Scanner;

public class One {

    // Input [1 2 4 3 5 0] -> Output [0 1 2 3 4 5]
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        int size = sc.nextInt();
        Integer[] arr = new Integer[size];
        for (int i = 0; i < size; i++) {
            arr[i] = sc.nextInt();
        }

        selectionSort(arr);
    }

    private static void selectionSort(Comparable[] a) {
        for (int i = 0; i < a.length; i++) {
            int min = i;
            // find smallest value
            for (int j = i + 1; j < a.length; j++) {
                if (less(a[j], a[min])) {
                    min = j;
                }
            }

            swap(a, i, min);
            for (Comparable current : a) {
                System.out.print(current + " ");
            }
            System.out.println();
        }
    }

    private static boolean less(Comparable v, Comparable w) {
        return v.compareTo(w) < 0;
    }

    private static void swap(Comparable[] a, int j, int k) {
        Comparable swap = a[j];
        a[j] = a[k];
        a[k] = swap;
    }

}
