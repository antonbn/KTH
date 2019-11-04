import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

public class NodePolyline implements Polyline {
    private static class Node {
        public Point vertex;
        public Node nextNode;

        public Node(Point vertex) {
            this.vertex = vertex;
            nextNode = null;
        }
    }

    private Node vertices;
    private String color = "black";
    private int width = 1;

    public NodePolyline(Point[] vertices) {
        if (vertices.length > 0) {
            Node node = new Node(new Point(vertices[0]));
            this.vertices = node;
            int pos = 1;
            while (pos < vertices.length) {
                node.nextNode = new Node(new Point(vertices[pos++]));
                node = node.nextNode;
            }
        }
    }

    public String toString() {
        StringBuilder output = new StringBuilder("{[");
        Node currentNode = vertices;

        while (currentNode != null) {
            output.append(currentNode.vertex.toString());
            currentNode = currentNode.nextNode;
        }

        return output.toString() + "], " + color + ", " + width + "}";
    }

    @Override
    public Point[] getVertices() {
        ArrayList<Point> verticesList = new ArrayList<>();
        iterator().forEachRemaining(verticesList::add);

        return verticesList.toArray(new Point[verticesList.size()]);
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

        Node currentNode = vertices;

        while (currentNode.nextNode != null) {
            length += currentNode.vertex.distance(currentNode.nextNode.vertex);
            currentNode = currentNode.nextNode;
        }

        return length;
    }

    @Override
    public void add(Point vertex) {
        Node currentNode = vertices;

        if (currentNode == null) {
            vertices = new Node(vertex);
        } else {
            // Iterate to end
            while (currentNode.nextNode != null) {
                currentNode = currentNode.nextNode;
            }

            currentNode.nextNode = new Node(vertex);
        }
    }

    @Override
    public void insertBefore(Point vertex, String vertexName) {
        Node currentNode = vertices;
        Node nodeToBeAdded = new Node(vertex);

        // If vertex should be added at start
        if (vertexName.equals(currentNode.vertex.getName())) {
            nodeToBeAdded.nextNode = currentNode;
            vertices = nodeToBeAdded;
        } else {
            while (currentNode.nextNode != null) {
                if (vertexName.equals(currentNode.nextNode.vertex.getName())) {
                    nodeToBeAdded.nextNode = currentNode.nextNode;
                    currentNode.nextNode = nodeToBeAdded;
                    break;
                }
                currentNode = currentNode.nextNode;
            }
        }
    }

    @Override
    public void remove(String vertexName) {
        Node currentNode = vertices;

        // If vertex should be removed at start
        if (vertexName.equals(currentNode.vertex.getName())) {
            vertices = currentNode.nextNode;
        } else {
            while (currentNode.nextNode != null) {
                if (vertexName.equals(currentNode.nextNode.vertex.getName())) {
                    currentNode.nextNode = currentNode.nextNode.nextNode;
                    break;
                }
                currentNode = currentNode.nextNode;
            }
        }
    }

    @Override
    public Iterator<Point> iterator() {
        return new Iterator<Point>() {
            Node currentNode = vertices;

            @Override
            public boolean hasNext() {
                return currentNode != null;
            }

            @Override
            public Point next() {
                Point vertex = currentNode.vertex;
                currentNode = currentNode.nextNode;

                return vertex;
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }
}
