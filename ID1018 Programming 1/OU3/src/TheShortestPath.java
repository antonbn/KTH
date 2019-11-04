class TheShortestPath {
    static int[] intermediateStations(double[] a, double[][] b, double[] c) {
        int[] intermediateStations = new int[2];
        double minDistance = Double.MAX_VALUE;

        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < b[i].length; j++) {
                if (a[i] + b[i][j] + c[j] < minDistance) {
                    minDistance = a[i] + b[i][j] + c[j];
                    intermediateStations[0] = i;
                    intermediateStations[1] = j;
                }
            }
        }

        return intermediateStations;
    }

    static double lengthOfShortestPath(double[] a, double[][] b, double[] c) {
        double minDistance = Double.MAX_VALUE;

        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < b[i].length; j++) {
                if (a[i] + b[i][j] + c[j] < minDistance) {
                    minDistance = a[i] + b[i][j] + c[j];
                }
            }
        }

        return minDistance;
    }
}