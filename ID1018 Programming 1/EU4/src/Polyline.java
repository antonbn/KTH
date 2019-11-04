import java.util.Iterator;

public interface Polyline extends Iterable<Point> {
    Point[] getVertices();

    String getColor();

    int getWidth();

    double length();

    void setColor(String colour);

    void setWidth(int width);

    void add(Point vertex);

    void insertBefore(Point vertex, String vertexName);

    void remove(String vertexName);

    @Override
    Iterator<Point> iterator();
}