/** README
 ** The program takes in a file and counts which word occurs most frequently
 * in the file.
 ** The program goes through each word in the file and counts the number of occurrences,
 * it will then find the word with the most occurrences.
 ** The program is used through the command line. You should pass a filename
 * as argument, it will then print the word with most occurrences.
 * Author: Anton Bothin
 * Date: 2018-09-26
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Two {

    public static void main(String[] args) throws FileNotFoundException {
        int minlen = Integer.parseInt(args[0]); // key-length cutoff

        // Takes the file specified as input
        File file = new File(args[1]);
        Scanner sc = new Scanner(file);

        // Number of words to read
        int N = 800;

        BST<String, Integer> st = new BST<>();
        //BinarySearchST<String, Integer> st = new BinarySearchST<>();
        while (sc.hasNext() && N > 0)
        { // Build symbol table and count frequencies.
            String word = sc.next();
            if (word.length() < minlen) continue; // Ignore short keys.
            if (!st.contains(word)) st.put(word, 1);
            else st.put(word, st.get(word) + 1);

            N--;
        }

        long startTime = System.nanoTime();
        // Find a key with the highest frequency count.
        String max = "";
        st.put(max, 0);
        for (String word : st.keys())
            if (st.get(word) > st.get(max))
                max = word;

        long endTime   = System.nanoTime();
        long totalTime = endTime - startTime;
        System.out.println("Time taken: " + totalTime + " ns");

        System.out.println(max + " " + st.get(max));
    }

}


