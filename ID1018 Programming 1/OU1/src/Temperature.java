import java.util.Locale;
import java.util.Scanner;

class Temperature {
    private int numberOfWeeks;
    private int measurementsPerWeek;
    private double[][] tempMeasurements;

    Temperature() {
        System.out.println("TEMPERATURER\n");

        getMeasurementsFromUser();
        printMeasurementsPerWeek();
        calculateTemperatureDataPerWeek();
    }

    private void getMeasurementsFromUser() {
        Scanner in = new Scanner(System.in);
        in.useLocale(Locale.US);

        System.out.print("Antalet veckor: ");
        numberOfWeeks = in.nextInt();

        System.out.print("Antalet mätningar per vecka: ");
        measurementsPerWeek = in.nextInt();

        tempMeasurements = new double[numberOfWeeks][measurementsPerWeek];

        for (int week = 0; week < numberOfWeeks; week++) {
            System.out.println("Temperaturer - vecka " + (week + 1) + ":");
            for (int measurement = 0; measurement < measurementsPerWeek; measurement++) {
                tempMeasurements[week][measurement] = in.nextDouble();
            }
        }
    }

    private void printMeasurementsPerWeek() {
        System.out.println();
        System.out.println("Temperaturerna:");

        for (double[] measurementsForWeek : tempMeasurements) {
            for (double measurement : measurementsForWeek) {
                System.out.print(measurement + " ");
            }
            System.out.println();
        }
        System.out.println();
    }

    private void calculateTemperatureDataPerWeek() {
        // Generates an array of type TemperatureData with length numberOfWeeks
        TemperatureData[] tempDataPerWeek = new TemperatureData[numberOfWeeks];
        for (int i = 0; i < numberOfWeeks; i++) {
            tempDataPerWeek[i] = new TemperatureData();
        }
        TemperatureData tempDataAcrossWeeks = new TemperatureData();

        for (int week = 0; week < numberOfWeeks; week++) {
            for (double measurement : tempMeasurements[week]) {
                //Temperature data per week
                if (tempDataPerWeek[week].getMinTemp() > measurement) {
                    tempDataPerWeek[week].setMinTemp(measurement);
                }
                if (tempDataPerWeek[week].getMaxTemp() < measurement) {
                    tempDataPerWeek[week].setMaxTemp(measurement);
                }
                tempDataPerWeek[week].setSumTemp(tempDataPerWeek[week].getSumTemp() + measurement);

                //Temperature data across all weeks
                if (tempDataAcrossWeeks.getMinTemp() > measurement) {
                    tempDataAcrossWeeks.setMinTemp(measurement);
                }
                if (tempDataAcrossWeeks.getMaxTemp() < measurement) {
                    tempDataAcrossWeeks.setMaxTemp(measurement);
                }
                tempDataAcrossWeeks.setSumTemp(tempDataAcrossWeeks.getSumTemp() + measurement);
            }

            tempDataPerWeek[week].setAvgTemp(tempDataPerWeek[week].getSumTemp() / measurementsPerWeek);

            System.out.println("Data för vecka " + (week + 1) + ":");
            tempDataPerWeek[week].printData();
        }
        tempDataAcrossWeeks.setAvgTemp(tempDataAcrossWeeks.getSumTemp() / (numberOfWeeks * measurementsPerWeek));

        System.out.println("Data för alla veckor:");
        tempDataAcrossWeeks.printData();
    }
}
