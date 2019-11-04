/** README
 ** The program takes in a file and counts which word occurs most frequently
 * in the file, you specify how many words to show and from which rank.
 ** The program goes through each word in the file and counts the number of occurrences,
 * it will then find the words with the most occurrences.
 ** The program is used through the command line. You should pass a filename
 * as argument, after that you should specify two numbers: i and j. The program will then print
 * the words from with most occurrences the i:th word to the (i+j):th word.
 * Author: Anton Bothin
 * Date: 2018-09-27
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Scanner;

public class Three {

    public static void main(String[] args) throws FileNotFoundException {
        int minlen = Integer.parseInt(args[0]); // key-length cutoff

        // Takes the file specified as input
        File file = new File(args[1]);
        Scanner sc = new Scanner(file);

        BST<String, Integer> st = new BST<>();
        while (sc.hasNext()) {
            // Build symbol table and count frequencies.
            String word = sc.next();
            //if (word.length() < minlen) continue; // Ignore short keys.
            if (!st.contains(word)) st.put(word, 1);
            else st.put(word, st.get(word) + 1);
        }

        System.out.println("Specify two integers");
        sc = new Scanner(System.in);
        int begin = sc.nextInt();
        int length = sc.nextInt();

        fromIToJ(st, begin, begin + length);
    }

    private static void fromIToJ(BST<String, Integer> st, int i, int j) {
        // Find a key with the highest frequency count.
        String max[] = new String[j];
        Arrays.fill(max, "");

        st.put(max[0], 0);
        for (String word : st.keys()) {
            int pos = max.length;
            for (int k = 0; k < max.length; k++) {
                if (st.get(word) > st.get(max[k])) {
                    pos = k;
                    break;
                }
            }
            for (int k = max.length - 1; k > pos; k--) {
                max[k] = max[k-1];
            }
            if (pos < max.length) {
                max[pos] = word;
            }
        }

        for (int k = i; k < j; k++) {
            System.out.println(max[k] + " " + st.get(max[k]));
        }
    }

}


