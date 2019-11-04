/** README
 ** The program will take in a single integer representing the number of integers to sort. Then that
 * many integers are specified, the list is sorted while elements are added to it.
 ** The program iterates through the list each time an element is enqueued, it iterates until
 * the item you want to add is smaller then an item in the list. The enqueue method then adds the new
 * item to that spot.
 ** The program is used through the command line by typing in an integer representing how many numbers to
 * be sorted, you then write in that many numbers.
 * Author: Anton Bothin
 * Date: 2018-09-17
 */

import java.util.Scanner;

public class Seven {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        Queue queue = new Queue();

        int size = sc.nextInt();
        for (int i = 0; i < size; i++) {
            queue.enqueue(sc.nextInt());
        }
    }

}

class Queue {
    private Node first; // first element in list
    private int N; // number of items

    private class Node {
        int item;
        Node next;
    }

    public boolean isEmpty() {
        return N == 0;
    }

    public int size() {
        return N;
    }

    public void enqueue(int item) {
        if (N == 0) {
            first = new Node();
            first.item = item;
        } else if (item < first.item) {
            Node oldfirst = first;
            first = new Node();
            first.item = item;
            first.next = oldfirst;
        } else {
            Node priodNode = first;

            while (priodNode.next != null && priodNode.next.item < item) {
                priodNode = priodNode.next;
            }
            Node newNode = new Node();
            newNode.item = item;
            newNode.next = priodNode.next;
            priodNode.next = newNode;
        }
        N++;

        Node current = first;
        for (int i = 0; i < N; i++) {
            System.out.print(current.item + " ");
            current = current.next;
        }
        System.out.println();
    }

    public int dequeue() {
        // if the queue is empty there is nothing to dequeue
        if (isEmpty()) {
            return 0;
        }

        int item = first.item;
        first = first.next;

        N--;
        return item;
    }

}
