public final class Triangle {
    private Triangle() {

    }

    public static double calcPerimeter(double sideA, double sideB, double sideC) {
        return sideA + sideB + sideC;
    }

    public static double calcAreaOfTriangle(double side, double correspondingHeight) {
        return side * correspondingHeight;
    }

    public static double calcAreaOfTriangle(double sideA, double sideB, double sideC) {
        double semiPerimeter = calcPerimeter(sideA, sideB, sideC) / 2;

        return Math.sqrt(semiPerimeter * (semiPerimeter - sideA) * (semiPerimeter - sideB) * (semiPerimeter - sideC));
    }

    public static double calcLengthOfMedian(double sideA, double sideB, double medianSide) {
        return Math.sqrt(2 * sideA * sideA + 2 * sideB * sideB - medianSide * medianSide) / 2;
    }

    public static double calcLengthOfBisector(double sideA, double sideB, double radianAngle) {
        return (2 * sideA * sideB * Math.cos(radianAngle / 2)) / (sideA + sideB);
    }

    public static double calcRadiusOfCircumscribedCircle(double sideA, double sideB, double sideC) {
        double area = calcAreaOfTriangle(sideA, sideB, sideC);

        return (sideA * sideB * sideC) / (4 * area);
    }

    public static double calcRadiusOfInscribedCircle(double sideA, double sideB, double sideC) {
        double semiPerimeter = calcPerimeter(sideA, sideB, sideC) / 2;
        double area = calcAreaOfTriangle(sideA, sideB, sideC);

        return area / semiPerimeter;
    }
}
