import java.util.Iterator;
import java.util.NoSuchElementException;

public class Stack<Item> implements Iterable<Item> {
    private Node<Item> first;
    private int N;

    public boolean isEmpty() {
        return this.first == null;
    }

    public int size() {
        return this.N;
    }

    public void push(Item var1) {
        Stack.Node var2 = this.first;
        this.first = new Node<>();
        this.first.item = var1;
        this.first.next = var2;
        ++this.N;
    }

    public Item pop() {
        Item var1 = this.first.item;
        this.first = this.first.next;
        --this.N;
        return var1;
    }

    public Item get(int i) {
        int j = 0;
        for (Item item : this) {
            if (j == i) {
                return item;
            }
            j++;
        }

        return null;
    }

    private class Node<Item> {
        Item item;
        Node next;
    }

    public boolean contains(Item item) {
        for(Item i : this) {
            if (item.equals(i)) {
                return true;
            }
        }

        return false;
    }

    public Iterator<Item> iterator() {
        return new ListIterator<Item>(first);
    }

    // an iterator, doesn't implement remove() since it's optional
    private class ListIterator<Item> implements Iterator<Item> {
        private Node<Item> current;

        public ListIterator(Node<Item> first) {
            current = first;
        }

        public boolean hasNext() {
            return current != null;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }

        public Item next() {
            if (!hasNext()) throw new NoSuchElementException();
            Item item = current.item;
            current = current.next;
            return item;
        }
    }
}