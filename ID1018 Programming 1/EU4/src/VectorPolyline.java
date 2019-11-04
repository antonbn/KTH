import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.stream.Collectors;

public class VectorPolyline implements Polyline {
    private Point[] vertices;
    private String color = "black";
    private int width = 1;

    public VectorPolyline(Point[] vertices) {
        this.vertices = new Point[vertices.length];
        for (int i = 0; i < vertices.length; i++)
            this.vertices[i] = new Point(vertices[i]);
    }

    public String toString() {
        return "{[" + Arrays.stream(vertices).map(Point::toString).collect(Collectors.joining()) +
                "], " + color + ", " + width + "}";
    }

    @Override
    public Point[] getVertices() {
        return vertices.clone();
    }

    @Override
    public String getColor() {
        return color;
    }

    @Override
    public int getWidth() {
        return width;
    }

    @Override
    public void setColor(String color) {
        this.color = color;
    }

    @Override
    public void setWidth(int width) {
        this.width = width;
    }

    @Override
    public double length() {
        double length = 0;
        for (int i = 0; i < vertices.length - 1; i++) {
            length += vertices[i].distance(vertices[i + 1]);
        }
        return length;
    }

    @Override
    public void add(Point vertex) {
        Point[] h = new Point[this.vertices.length + 1];
        int i;
        for (i = 0; i < this.vertices.length; i++) {
            h[i] = this.vertices[i];
        }
        h[i] = new Point(vertex);
        this.vertices = h;
    }

    @Override
    public void insertBefore(Point vertex, String vertexName) {
        int occurrences = Collections.frequency(Arrays.stream(vertices)
                .map(Point::getName).collect(Collectors.toList()), vertexName);
        Point[] h = new Point[this.vertices.length + occurrences];

        int j = 0;
        for (Point vertexInPolyline : vertices) {
            if (vertexName.equals(vertexInPolyline.getName())) {
                h[j] = new Point(vertex);
                j++;
            }
            h[j] = vertexInPolyline;
            j++;
        }
        this.vertices = h;
    }

    @Override
    public void remove(String vertexName) {
        int occurrences = Collections.frequency(Arrays.stream(vertices)
                .map(Point::getName)
                .collect(Collectors.toList()), vertexName);
        Point[] h = new Point[this.vertices.length - occurrences];

        int j = 0;
        for (Point vertexInPolyline : vertices) {
            if (!vertexInPolyline.getName().equals(vertexName)) {
                h[j] = vertexInPolyline;
                j++;
            }
        }
        this.vertices = h;
    }

    @Override
    public Iterator<Point> iterator() {
        return Arrays.asList(vertices).iterator();
    }
}