/** README
 ** The program takes in a file and checks the hash code for each distinct word in the file,
 * it will the count how many times each hash code occur.
 ** The program goes through each word in the file, if the word has not yet occurred it
 * takes the hashcode and stores it in the ST to count the number of times that hash code
 * will be used.
 ** The program is used through the command line. You should pass a filename
 * as argument, it will then print all hash codes with 2 or more occurrences.
 * Author: Anton Bothin
 * Date: 2018-09-27
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Scanner;

public class Five {

    public static void main(String[] args) throws FileNotFoundException {
        // Takes the file specified as input
        File file = new File(args[0]);
        Scanner sc = new Scanner(file);

        BST<Integer, Queue<String>> bst = new BST<>();
        while (sc.hasNext()) {
            String word = sc.next();
            int hash = word.hashCode();

            if (!bst.contains(hash)) {
                bst.put(hash, new Queue<>());
            }
            if (!bst.get(hash).contains(word)) {
                bst.get(hash).enqueue(word);
            }
        }

        int distinct = 0;
        for (int hash : bst.keys()) {
            if (bst.get(hash).size() > 1) {
                System.out.println(bst.get(hash).toString() + " : " + hash);
            } else {
                distinct++;
            }
        }
        System.out.println(distinct + " distinct hash codes");
    }

}


