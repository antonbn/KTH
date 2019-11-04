/** README
 * The program will take in a string. Each character in this string will be enqueued unless
 * it is a '-' followed by a number, then it will dequeue the k:th element in the list where
 * k is the number inputted right after '-'.
 ** The implementation uses a single linked list. Dequeueing the k:th element means
 * iterating k times until the correct element is reached.
 ** The program is used through the command line by typing in a string and
 * then pressing enter.
 * Author: Anton Bothin
 * Date: 2018-09-11
 */

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Scanner;

class GeneralizedQueue<Item> implements Iterable<Item> {
    private Node first; // first element in list
    private int N; // number of items

    private class Node {
        Item item;
        Node next;
    }

    public boolean isEmpty() { return N == 0; }

    public int size() { return N; }

    public void enqueue(Item item) {
        Node oldfirst = first;
        first = new Node();
        first.item = item;
        first.next = oldfirst;
        N++;
    }

    public Item dequeue(final int k) {
        // If k == 1 then simply remove the first element
        if (k == 1) {
            Item item = first.item;
            first = first.next;
            N--;

            return item;
        }

        Node priorKthNode = new Node();
        // Temporarily puts it in front of the first element
        priorKthNode.next = first;
        // Iterate k times or until end of queue is reached
        for (int i = 1; i < k; i++) {
            priorKthNode = priorKthNode.next;
        }

        // gets the value of the k:th element
        Item item = priorKthNode.next.item;
        // relinking to remove the k:th element
        priorKthNode.next = priorKthNode.next.next;
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
        GeneralizedQueue<Character> queue = new GeneralizedQueue<>();

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
                int k = sc.nextInt();
                System.out.print(k + ": " + queue.dequeue(k) + " ");
            }
        }

        System.out.println();
        System.out.println(queue.toString());

        sc.close();
    }

}