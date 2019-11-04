import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

public class Polyline<T> {
    Point[] vertices;
    private String color = "black";
    private int width = 1;

    public Polyline() {
        this.vertices = new Point[0];
    }

    public Polyline(Point[] vertices) {
        this.vertices = new Point[vertices.length];
        for (int i = 0; i < vertices.length; i++)
            this.vertices[i] = new Point(vertices[i]);
    }

    public String toString() {
        return "{[" + Arrays.stream(vertices).map(Point::toString).collect(Collectors.joining()) +
                "], " + color + ", " + width + "}";
    }

    public Point[] getVertices() {
        return vertices.clone();
    }

    public String getColor() {
        return color;
    }

    public int getWidth() {
        return width;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public double length() {
        double length = 0;
        for (int i = 0; i < vertices.length - 1; i++) {
            length += vertices[i].distance(vertices[i + 1]);
        }
        return length;
    }

    public void addLast(Point vertex) {
        Point[] h = new Point[this.vertices.length + 1];
        int i;
        for (i = 0; i < this.vertices.length; i++)
            h[i] = this.vertices[i];
        h[i] = new Point(vertex);
        this.vertices = h;
    }

    public void addBefore(Point vertex, String vertexName) {
        int occurrencesOfName = Collections.frequency(Arrays.stream(vertices)
                .map(Point::getName)
                .collect(Collectors.toList()), vertexName);
        Point[] h = new Point[this.vertices.length + occurrencesOfName];

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

    public void remove(String vertexName) {
        int occurrencesOfName = Collections.frequency(Arrays.stream(vertices)
                .map(Point::getName)
                .collect(Collectors.toList()), vertexName);
        Point[] h = new Point[this.vertices.length - occurrencesOfName];

        int j = 0;
        for (Point vertexInPolyline : vertices) {
            if (!vertexInPolyline.getName().equals(vertexName)) {
                h[j] = vertexInPolyline;
                j++;
            }
        }
        this.vertices = h;
    }

    public class PolylineIterator {
        private int current = -1;

        public PolylineIterator() {
            if (Polyline.this.vertices.length > 0) {
                current = 0;
            }
        }

        public boolean hasVertex() {
            return current != -1;
        }

        public Point vertex() throws java.util.NoSuchElementException {
            if (!this.hasVertex()) {
                throw new java.util.NoSuchElementException(" end of iteration ");
            }
            Point vertex = Polyline.this.vertices[current];
            return vertex;
        }

        public void advance() {
            if (current >= 0 && current < Polyline.this.vertices.length - 1) {
                current++;
            } else {
                current = -1;
            }
        }
    }
}