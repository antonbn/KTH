class PolylineTest {
    public static void main(String[] args) {
        Point p1 = new Point("A", 3, 4);
        Point p2 = new Point("B", 5, 6);
        Point p3 = new Point("C", 1, 2);
        Point p4 = new Point("D", 2, 5);

        Point p5 = new Point("E", 2, 7);

        Point[] vertices = {p1, p2, p3, p4};

        //Polyline polyline = new VectorPolyline(vertices); // (1)
        Polyline polyline = new NodePolyline(vertices); // (2)

        System.out.println(polyline);

        polyline.setColor("red");
        polyline.setWidth(2);

        System.out.println("Length: " + polyline.length());

        polyline.remove("A");
        polyline.insertBefore(p5, "C");
        polyline.add(p5);
        System.out.println(polyline);

        System.out.println("\nPolyline iterator: ");
        for (Point vertex : polyline) {
            System.out.println(vertex);
        }
    }
}
