/** README
 ** The program will take in a string. Each character in this string will be enqueued or
 * dequeued. '-' will dequeue the first element, '+' will dequeue the last element.
 * '*' followed by any character will enqueue that character at the front of the queue,
 * any other character will be enqueued at the end of the list.
 ** The implementation uses a circular single linked list keeping track of both the first and last
 * element.
 ** The program is used through the command line by typing in a string and
 * then pressing enter.
 * Author: Anton Bothin
 * Date: 2018-09-10
 */

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Scanner;

class CircularQueue<Item> implements Iterable<Item> {
    private Node first; // first element in list
    private Node last; // last element in list
    private int N; // number of items

    private class Node {
        Item item;
        Node next;
    }

    public boolean isEmpty() { return N == 0; }

    public int size() { return N; }

    public void addFirst(Item item) {
        // if the queue is empty
        if (size() == 0) {
            first = new Node();
            first.item = item;
            // circular queue
            first.next = first;
            last = first;
        } else {
            Node oldfirst = first;
            first = new Node();
            first.item = item;
            first.next = oldfirst;
            last.next = first;
        }
        N++;
    }

    public void addLast(Item item) {
        // if the queue is empty
        if (size() == 0) {
            first = new Node();
            first.item = item;
            // circular queue
            first.next = first;
            last = first;
        } else {
            Node oldlast = last;
            last = new Node();
            last.item = item;
            last.next = first;
            oldlast.next = last;
        }
        N++;
    }

    public Item removeFirst() {
        Item item = first.item;
        first = first.next;
        last.next = first;
        N--;
        if (isEmpty()) {
            first = last = null;
        }
        return item;
    }

    public Item removeLast() {
        Item item = last.item;

        // find the element just before the last one.
        Node newlast = first;
        while (newlast.next != last) {
            newlast = newlast.next;
        }
        last = newlast;
        last.next = first;
        N--;
        if (isEmpty()) {
            first = last = null;
        }

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
            // Needed since the list is circular
            boolean looped = false;

            @Override
            public boolean hasNext() {
                return current != null && !looped;
            }

            @Override
            public Item next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                } else {
                    Item item = current.item;
                    current = current.next;
                    if (current == first) {
                        looped = true;
                    }

                    return item;
                }
            }
        };
    }

    public static void main(String[] args) {
        CircularQueue<Character> queue = new CircularQueue<>();

        Scanner sc = new Scanner(System.in);
        // Makes the scanner only read one character at a time
        sc.useDelimiter("");

        while (true) {
            // Read a character
            char c = sc.next().charAt(0);
            if (c == '\n') {
                break;
            } else if (c == '-') {
                System.out.print(queue.removeFirst() + " ");
            } else if (c == '+') {
                System.out.print(queue.removeLast() + " ");
            } else if (c == '*') {
                c = sc.next().charAt(0);
                queue.addFirst(c);
            } else {
                queue.addLast(c);
            }
        }

        System.out.println();
        System.out.println(queue.toString());

        sc.close();
    }

}