class PointTest {
    public static void main(String[] args) {
        Point p1 = new Point("A", 3, 4);
        Point p2 = new Point("B", 5, 6);
        System.out.println(p1 + " " + p2);

        String n = p1.getName();
        int x = p1.getX();
        int y = p1.getY();
        System.out.println(n + " " + x + " " + y);

        double d = p1.distance(p2);
        System.out.println(d);
        boolean b = p1.equals(p2);
        System.out.println(b);

        p2.setX(1);
        p2.setY(2);
        System.out.println(p2);

        Point p = new Point(p1);
        System.out.println(p);
    }
}
