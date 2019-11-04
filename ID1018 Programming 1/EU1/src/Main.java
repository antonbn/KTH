public class Main {

    public static void main(String[] args) {
        int[] elements = {0, 1, 2, 3, 4, 5, 6, 7, 8, -7, -6, -5, -4, -3, -2, -1};
        //int[] elements = new int[19];
        //elements[16] = -1;

        System.out.println(min(elements));
    }

    // The min method returns the least element in a sequential
    // collection . If the collection is empty an
    // IllegalArgumentException is thrown .
    public static int min(int[] elements) throws IllegalArgumentException {
        if (elements.length == 0)
            throw new IllegalArgumentException(" empty collection ");
        // Is used in trace printing 2:
        //int nofIters = 1;
        int[] sequence = elements;
        int nofPairs = sequence.length / 2;
        int nofUnpairedElements = sequence.length % 2;
        int nofPossibleElements = nofPairs + nofUnpairedElements;
        int[] partialSeq;
        int i;
        int j;
        partialSeq = new int[nofPossibleElements];

        while (nofPairs > 0) { // FÖRSTA ÄNDRING
            // extract a partial sequence of possible elements
            i = 0;
            j = 0;
            while (j < nofPairs) {
                partialSeq[j++] = (sequence[i] < sequence[i + 1]) ?
                        sequence[i] : sequence[i + 1];
                i += 2;
            }
            if (nofUnpairedElements == 1) {
                partialSeq[j] = sequence[i]; // ANDRA ÄNDRING
            }
            // now turn to the partial sequence
            sequence = partialSeq;
            nofPairs = nofPossibleElements / 2;
            nofUnpairedElements = nofPossibleElements % 2;
            nofPossibleElements = nofPairs + nofUnpairedElements;
            // Trace printing 1 - to follow the sequence
            System.out.println(java.util.Arrays.toString(sequence));
            // Trace printing 2 - to terminate the loop preemptively
            // ( to be able to see what happens initially )
            //if (nofIters++ == 10)
            //    System.exit(0);
        }
        // sequence [0] is the only remaining possible element
        // - it is the least element
        return sequence[0];
    }

    public static int min2(int[] elements) throws IllegalArgumentException {
        if (elements.length == 0) {
            throw new IllegalArgumentException(" empty collection ");
        }

        int min = elements[0];

        for (int element : elements) {
            if (element < min) {
                min = element;
            }
        }

        return min;
    }
}
