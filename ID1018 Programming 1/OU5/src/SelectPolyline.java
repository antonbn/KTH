import java.util.Arrays;
import java.util.Random;

class SelectPolyline {
    public static final Random rand = new Random();
    public static final int NOF_POLYLINES = 10;

    public static void main(String[] args) {
        // Create a random number of polylines
        Polyline[] polylines = new Polyline[NOF_POLYLINES];
        for (int i = 0; i < NOF_POLYLINES; i++) {
            polylines[i] = randomPolyline();
        }

        Polyline shortestPolyline = polylines[0];
        // Determine the shortest yellow polyline
        for (Polyline polyline : polylines) {
            System.out.println(polyline + ", length: " + polyline.length());

            if (!"yellow".equals(shortestPolyline.getColor()) ||
                    "yellow".equals(polyline.getColor()) && polyline.length() < shortestPolyline.length()) {
                shortestPolyline = polyline;
            }
        }
        System.out.println("\nShortest yellow polyline: " + shortestPolyline + ", length: " + shortestPolyline.length());
    }

    public static Point randomPoint() {
        String n = "" + (char) (65 + rand.nextInt(26));
        int x = rand.nextInt(11);
        int y = rand.nextInt(11);
        return new Point(n, x, y);
    }

    public static Polyline randomPolyline() {
        Polyline polyline = new Polyline();
        String[] possibleColors = {"blue", "red", "yellow"};

        int width = 1 + rand.nextInt(10);
        String color = possibleColors[rand.nextInt(3)];

        polyline.setWidth(width);
        polyline.setColor(color);

        int nrOfVertices = 2 + rand.nextInt(7);
        int nrOfSelectedVertices = 0;

        while (nrOfSelectedVertices < nrOfVertices) {
            Point chosenPoint = randomPoint();
            final String chosenName = chosenPoint.getName();
            // Two vertices can not have the same name
            if (Arrays.stream(polyline.getVertices()).noneMatch(point -> point.getName().equals(chosenName))) {
                polyline.addLast(chosenPoint);
                nrOfSelectedVertices++;
            }
        }

        return polyline;
    }
}
