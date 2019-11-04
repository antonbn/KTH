import java.util.Scanner;

import static java.lang.System.out;

class OperationsWithNaturalNumbersGivenAsStrings {
    public static void main(String[] args) {
        out.println("OPERATIONS ON NATURAL NUMBERS IN CHARACTER STRINGS");

        Scanner in = new Scanner(System.in);
        out.println("Two natural numbers: ");
        String num1 = in.next();
        String num2 = in.next();
        out.println();

        out.println("Operator: (+/-)");
        char operator = in.next().charAt(0);

        String result;
        if (operator == '+') {
            result = add(num1, num2);
        } else {
            result = subtract(num1, num2);
        }
        show(num1, num2, result, operator);
    }

    public static String add(String num1, String num2) {
        // Make num1 and num2 be equally long
        int maxLen = Math.max(num1.length(), num2.length());
        num1 = insertCharAtStart(num1, maxLen - num1.length(), '0');
        num2 = insertCharAtStart(num2, maxLen - num2.length(), '0');

        StringBuilder sb = new StringBuilder();
        int carry = 0;

        for (int i = maxLen - 1; i >= 0; i--) {
            int tempNum1 = Character.getNumericValue(num1.charAt(i));
            int tempNum2 = Character.getNumericValue(num2.charAt(i));

            // Calculate sum at position i
            int sum = tempNum1 + tempNum2 + carry;
            carry = sum / 10;
            sum -= 10 * carry;

            sb.insert(0, sum);
        }
        if (carry > 0) {
            sb.insert(0, carry);
        }

        return sb.toString();
    }

    public static String subtract(String num1, String num2) {
        // Make num1 and num2 be equally long
        int maxLen = Math.max(num1.length(), num2.length());
        num1 = insertCharAtStart(num1, maxLen - num1.length(), '0');
        num2 = insertCharAtStart(num2, maxLen - num2.length(), '0');

        StringBuilder sb = new StringBuilder();
        int carry = 0;
        boolean nonzeroNumAtFirstPos = false;

        for (int i = maxLen - 1; i >= 0; i--) {
            int tempNum1 = Character.getNumericValue(num1.charAt(i));
            int tempNum2 = Character.getNumericValue(num2.charAt(i));

            // Calculate difference at position i
            int difference = tempNum1 - tempNum2 - carry;
            carry = difference < 0 ? 1 : 0;
            difference += carry * 10;

            if (difference > 0 || i == 0 || nonzeroNumAtFirstPos) {
                nonzeroNumAtFirstPos = true;
                sb.insert(0, difference);
            }
        }

        return sb.toString();
    }

    public static void show(String num1, String num2, String result, char operator) {
        int len1 = num1.length();
        int len2 = num2.length();
        int len = result.length();
        int maxLen = Math.max(Math.max(len1, len2), len);

        num1 = insertCharAtStart(num1, maxLen - len1, ' ');
        num2 = insertCharAtStart(num2, maxLen - len2, ' ');
        result = insertCharAtStart(result, maxLen - len, ' ');

        out.println("  " + num1);
        out.println(operator + " " + num2);
        for (int i = 0; i < maxLen + 2; i++) {
            out.print(" -");
        }
        out.println();
        out.println("  " + result + "\n");
    }

    public static String insertCharAtStart(String s, int nofSpaces, char c) {
        StringBuilder sb = new StringBuilder(s);
        for (int i = 0; i < nofSpaces; i++) {
            sb.insert(0, c);
        }
        return sb.toString();
    }
}