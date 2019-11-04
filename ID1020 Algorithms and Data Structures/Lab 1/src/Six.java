/** README
 * The program will take in a filename. Each character in this file will be read as
 * standard input. The program checks so that all types of parentheses are properly balanced.
 ** The implementation uses a stack. If a start parentheses is read it is added to the
 * stack. If an end parentheses is read then the stack is popped checking that the
 * last element added was the corresponding start parentheses.
 ** The program is used through the command line by typing in a filename and
 * then pressing enter.
 * Author: Anton Bothin
 * Date: 2018-09-11
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Iterator;
import java.util.Scanner;

public class Six {
    public static void main(String[] args) throws FileNotFoundException {
        Stack<Character> stack = new Stack<>();
        stack.push(' '); // In case an end parentheses occurs first, there should still be something to pop
        boolean isBalanced = true;

        // Takes the file specified as input
        File file = new File(args[0]);
        Scanner sc = new Scanner(file);
        // Makes the scanner only read one character at a time
        sc.useDelimiter("");

        // Read until EOF
        while (sc.hasNext()) {
            char c = sc.next().charAt(0);
            if (c == '{' || c == '[' || c == '(') {
                stack.push(c);
            } else if (c == '}' && stack.pop() != '{') {
                System.err.println("'}' occurred without a corresponding '{'");
                isBalanced = false;
                break;
            } else if (c == ']' && stack.pop() != '[') {
                System.err.println("']' occurred without a corresponding '['");
                isBalanced = false;
                break;
            } else if (c == ')' && stack.pop() != '(') {
                System.err.println("')' occurred without a corresponding '('");
                isBalanced = false;
                break;
            }
        }
        sc.close();

        if (isBalanced) {
            System.out.println("The parentheses in the file are balanced.");
        }
    }

}

// Class taken from the book
class Stack<Item> {
    private Node first; // top of stack (most recently added node)
    private int N; // number of items

    private class Node
    { // nested class to define nodes
        Item item;
        Node next;
    }
    public boolean isEmpty() { return first == null; } // Or: N == 0.

    public int size() { return N; }

    public void push(Item item)
    { // Add item to top of stack.
        Node oldfirst = first;
        first = new Node();
        first.item = item;
        first.next = oldfirst;
        N++;
    }
    public Item pop()
    { // Remove item from top of stack.
        Item item = first.item;
        first = first.next;
        N--;
        return item;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder("");
        Node current = first;
        for (int i = 0; i < size(); i++) {
            sb.append("[");
            sb.append(current.item);
            sb.append("]");
            if (i < size() - 1) {
                sb.append(", ");
            }
            current = current.next;
        }

        return sb.toString();
    }

}