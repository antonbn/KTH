class TemperatureData {
    private double minTemp;
    private double maxTemp;
    private double sumTemp;
    private double avgTemp;

    TemperatureData() {
        minTemp = Double.MAX_VALUE;
        maxTemp = Double.MIN_VALUE;
        sumTemp = 0;
        avgTemp = 0;
    }

    void printData() {
        System.out.println("Minsta: " + minTemp);
        System.out.println("Största: " + maxTemp);
        System.out.println("Summan: " + sumTemp);
        System.out.println("Medel: " + avgTemp);
        System.out.println();
    }

    double getMinTemp() {
        return minTemp;
    }

    void setMinTemp(double minTemp) {
        this.minTemp = minTemp;
    }

    double getMaxTemp() {
        return maxTemp;
    }

    void setMaxTemp(double maxTemp) {
        this.maxTemp = maxTemp;
    }

    double getSumTemp() {
        return sumTemp;
    }

    void setSumTemp(double sumTemp) {
        this.sumTemp = sumTemp;
    }

    double getAvgTemp() {
        return avgTemp;
    }

    void setAvgTemp(double avgTemp) {
        this.avgTemp = avgTemp;
    }
}
