import java.util.Arrays;
import java.util.List;
import java.util.Random;

class Polylines {
    public static final Random rand = new Random();
    public static final int NOF_POLYLINES = 10;

    public static void main(String[] args) {
        // Create a random number of polylines
        Polyline[] polylines = new Polyline[NOF_POLYLINES];
        for (int i = 0; i < NOF_POLYLINES; i++) {
            try {
                polylines[i] = randomPolyline();
            } catch (IllegalAccessException | InstantiationException ex) {
                System.out.println(ex);
                i--;
            }
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

    public static Polyline randomPolyline() throws InstantiationException, IllegalAccessException {
        List<Class<? extends Polyline>> polylineTypes = Arrays.asList(VectorPolyline.class, NodePolyline.class);
        String[] possibleColors = {"blue", "red", "yellow"};

        //Initialize polyline either as a VectorPolyline or a NodePolyline
        Polyline polyline = polylineTypes.get(rand.nextInt(polylineTypes.size())).newInstance();

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
                polyline.add(chosenPoint);

                nrOfSelectedVertices++;
            }
        }

        return polyline;
    }
}
