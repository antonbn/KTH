import org.junit.Test;

import java.util.Random;

import static org.junit.Assert.*;

public class MainTest {
    @Test
    public void bothMinMethodsReturnSameValue() {
        int[] elements = generateRandomIntSequence();

        assertEquals(Main.min2(elements), Main.min(elements));
    }

    public int[] generateRandomIntSequence() {
        Random rand = new Random();

        int[] sequence = new int[rand.nextInt(100) + 1];
        for (int i = 0; i < sequence.length; i++) {
            sequence[i] = rand.nextInt();
        }

        return sequence;
    }
}