/** README
 * The program will take in a string. Each character in this string will be enqueued unless
 * it is a '-', then it will dequeue the first character added.
 * The implementation uses a loop to constantly check if a character is
 ** inputed. It will store this character in a queue and read the next
 ** character until a newline is read. If the character is a '-' the program
 * will dequeue the first character.
 * The program is used through the command line by typing in a string and
 ** then pressing enter.
 * Author: Anton Bothin
 * Date: 2018-09-10
 */

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Scanner;

class Queue<Item> implements Iterable<Item> {
    private Node first; // first node in queue
    private Node last; // last node in queue
    private int N; // number of items

    private class Node {
        Item item;
        Node previous;
        Node next;
    }

    public boolean isEmpty() { return first == null; }

    public int size() { return N; }

    // Add item to queue
    public void enqueue(Item item) {
        // if the queue is empty
        if (size() == 0) {
            last = new Node();
            last.item = item;
            first = last;
        } else {
            Node oldlast = last;
            last = new Node();
            last.item = item;
            last.previous = oldlast;
            oldlast.next = last;
        }
        N++;
    }

    // Remove item from queue
    public Item dequeue() {
        // if the queue is empty there is nothing to dequeue
        if (isEmpty()) {
            return null;
        }

        Item item = first.item;
        first = first.next;
        // otherwise there is no previous
        if (size() > 1) {
            first.previous = null;
        }
        N--;
        return item;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder("");
        int counter = 0;
        for (Item item : this) {
            sb.append("[");
            sb.append(item);
            sb.append("]");
            if (counter < N - 1) { // do not have a comma after the last element
                sb.append(", ");
            }
            counter++;
        }

        return sb.toString();
    }

    @Override
    public Iterator<Item> iterator() {
        return new Iterator<Item>() {
            Node current = first;

            @Override
            public boolean hasNext() {
                return current != null;
            }

            @Override
            public Item next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                } else {
                    Item item = current.item;
                    current = current.next;
                    return item;
                }
            }
        };
    }

    public static void main(String[] args) {
        Queue<Character> queue = new Queue<>();

        Scanner sc = new Scanner(System.in);
        // Makes the scanner only read one character at a time
        sc.useDelimiter("");

        while (true) {
            // Read a character
            char c = sc.next().charAt(0);
            if (c == '\n') {
                break;
            } else if (c != '-') {
                queue.enqueue(c);
            } else {
                System.out.print(queue.dequeue() + " ");
            }
        }

        System.out.println();
        System.out.println(queue.toString());

        sc.close();
    }

}
