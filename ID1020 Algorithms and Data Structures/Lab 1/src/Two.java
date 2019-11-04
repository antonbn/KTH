/** README
 * The program will take in a string as input and output it in reverse.
 * The implementation uses a loop to constantly check if a character is
 ** inputed. It will store this character in a stack and read the next
 ** character until a newline is read. The program will then output this
 ** array in reverse order.
 * The program is used through the command line by typing in a string and
 ** then pressing enter.
 * Author: Anton Bothin
 * Date: 2018-09-10
 */


import java.util.Iterator;
import java.util.Scanner;

public class Two {

    public static void main(String[] args){
        StackADT<Character> stack = new StackADT<>();

        Scanner sc = new Scanner(System.in);
        // Makes the scanner only read one character at a time
        sc.useDelimiter("");

        while (true) {
            // Read a character
            char c = sc.next().charAt(0);
            if (c == '\n') {
                break;
            }
            stack.push(c);
        }
        sc.close();

        System.out.println(stack.toString());
        while (!stack.isEmpty()) {
            System.out.print(stack.pop());
        }
        System.out.println();
    }
}

// Class taken from the book, toString implemented
class StackADT<Item> {
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

