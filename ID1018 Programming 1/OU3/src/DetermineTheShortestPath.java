import java.util.Locale;
import java.util.Scanner;

public class DetermineTheShortestPath {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        in.useLocale(Locale.US);

        System.out.println("Antalet mellanstationer i zon Z2:");
        int numZone2Stations = in.nextInt();
        System.out.println("Antalet mellanstationer i zon Z3:");
        int numZone3Stations = in.nextInt();

        double[] a = new double[numZone2Stations];
        double[][] b = new double[numZone2Stations][numZone3Stations];
        double[] c = new double[numZone3Stations];

        System.out.println();
        System.out.println("Längden mellan station X och station Ui");
        for (int i = 0; i < numZone2Stations; i++){
            a[i] = in.nextDouble();
        }

        for (int i = 0; i < numZone2Stations; i++){
            System.out.println("Längden mellan station U" + (i + 1) + " och station Vj");
            for (int j = 0; j < numZone3Stations; j++) {
                b[i][j] = in.nextDouble();
            }
        }

        System.out.println("Längden mellan station Vj och station Y");
        for (int j = 0; j < numZone3Stations; j++) {
            c[j] = in.nextDouble();
        }

        int[] intermediateStations = TheShortestPath.intermediateStations(a, b, c);
        double shortestLength = TheShortestPath.lengthOfShortestPath(a, b, c);

        System.out.println();
        System.out.println("Kortaste vägen: " + shortestLength);
        System.out.println("Mellanstationer: " + (intermediateStations[0] + 1) + ", " + (intermediateStations[1] + 1));
    }
}
