import java.util.Locale;
import java.util.Scanner;

public class TrianglesAndTheirCircles {
    public static void main(String[] args) {
        System.out.println("EN TRIANGEL OCH DESS CIRKLAR");
        System.out.println();

        Scanner in = new Scanner(System.in);
        in.useLocale(Locale.US);
        System.out.println("Skriv in sidornas längder:");

        double[] lengths = new double[3];
        for (int i = 0; i < lengths.length; i++) {
            lengths[i] = in.nextDouble();
        }

        double circumscribedCircleRadius = Triangle.calcRadiusOfCircumscribedCircle(lengths[0], lengths[1], lengths[2]);
        double inscribedCircleRadius = Triangle.calcRadiusOfInscribedCircle(lengths[0], lengths[1], lengths[2]);

        System.out.println();
        System.out.println("Omskrivna cirkelns radie: " + circumscribedCircleRadius);
        System.out.println("Inskrivna cirkelns radie: " + inscribedCircleRadius);
    }
}
