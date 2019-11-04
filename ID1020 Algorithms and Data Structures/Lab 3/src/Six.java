/** README
 ** The program takes in a file and stores each position of each word in the file.
 ** The program goes through each word in the file and stores the position where the position is
 * the number of characters before it.
 ** The program is used through the command line. You should pass a filename
 * as argument and then specify a word, if the word exists it will print all positions of
 * that word.
 * Author: Anton Bothin
 * Date: 2018-09-27
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.regex.Pattern;

public class Six {

    public static void main(String[] args) throws FileNotFoundException {
        // Takes the file specified as input
        File file = new File(args[0]);
        Scanner sc = new Scanner(file);

        // Key: Word, Value: List of positions
        BST<String, ArrayList<Integer>> bst = new BST<>();
        int position = 0;
        while (sc.hasNextLine()) {
            String line = sc.nextLine();
            Scanner scLine = new Scanner(line);
            while (scLine.hasNext()) {
                String word = scLine.next();
                String before = line.split(Pattern.quote(word), 2)[0];
                line = line.split(Pattern.quote(word), 2)[1];

                position += before.length();

                if (!bst.contains(word)) {
                    bst.put(word, new ArrayList<>());
                }
                bst.get(word).add(position);
                position += word.length();
            }
        }

        sc = new Scanner(System.in);
        System.out.println("On which positions in the text will you find the word ");
        String word = sc.next();

        ArrayList<Integer> positions = bst.get(word);
        System.out.println("Positions: " + positions.toString());
    }

}


