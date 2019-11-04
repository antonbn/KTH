import java.util.Scanner;

/**
 * The {@code Graph} class represents an undirected graph of vertices
 * named 0 through <em>V</em> – 1.
 * It supports the following two primary operations: add an edge to the graph,
 * iterate over all of the vertices adjacent to a vertex. It also provides
 * methods for returning the number of vertices <em>V</em> and the number
 * of edges <em>E</em>. Parallel edges and self-loops are permitted.
 * By convention, a self-loop <em>v</em>-<em>v</em> appears in the
 * adjacency list of <em>v</em> twice and contributes two to the degree
 * of <em>v</em>.
 * <p>
 * This implementation uses an adjacency-lists representation, which
 * is a vertex-indexed array of {@link Bag} objects.
 * All operations take constant time (in the worst case) except
 * iterating over the vertices adjacent to a given vertex, which takes
 * time proportional to the number of such vertices.
 * <p>
 * For additional documentation, see <a href="https://algs4.cs.princeton.edu/41graph">Section 4.1</a>
 * of <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 * @author Robert Sedgewick
 * @author Kevin Wayne
 */
public class Graph {
    private final int V;
    private int E;
    private Bag<Integer>[] adj;
    private RedBlackBST<String, Integer> conversion;

    /**
     * Initializes a graph from the specified input stream.
     * The format is the number of vertices <em>V</em>,
     * followed by the number of edges <em>E</em>,
     * followed by <em>E</em> pairs of vertices, with each entry separated by whitespace.
     *
     * @param  V the number of vertices
     * @param in the input stream
     * @throws IllegalArgumentException if the endpoints of any edge are not in prescribed range
     * @throws IllegalArgumentException if the number of vertices or edges is negative
     * @throws IllegalArgumentException if the input stream is in the wrong format
     */
    public Graph(int V, Scanner in) {
        this.V = V;
        if (V < 0) throw new IllegalArgumentException("number of vertices in a Graph must be nonnegative");
        adj = (Bag<Integer>[]) new Bag[V];
        for (int v = 0; v < V; v++) {
            adj[v] = new Bag<>();
        }

        E = 0;
        int N = 0;
        conversion = new RedBlackBST<>();
        while (in.hasNextLine()) {
            String line = in.nextLine();
            Scanner lineSc = new Scanner(line);
            String vStr = lineSc.next();
            String wStr = lineSc.next();

            N = addConversion(vStr, N);
            N = addConversion(wStr, N);

            int v = conversion.get(vStr);
            int w = conversion.get(wStr);
            addEdge(v, w);
        }
    }

    private int addConversion(String key, int value) {
        if (!conversion.contains(key)) {
            conversion.put(key, value);
            value++;
        }

        return value;
    }

    public String getConversion(int value) {
        for (String key : conversion.keys()) {
            if (conversion.get(key).equals(value)) {
                return key;
            }
        }

        return null;
    }

    public int V() {
        return V;
    }

    /**
     * Adds the undirected edge v-w to this graph.
     *
     * @param v one vertex in the edge
     * @param w the other vertex in the edge
     * @throws IllegalArgumentException unless both {@code 0 <= v < V} and {@code 0 <= w < V}
     */
    public void addEdge(int v, int w) {
        E++;
        adj[v].add(w);
        adj[w].add(v);
    }

    /**
     * Returns the vertices adjacent to vertex {@code v}.
     *
     * @param v the vertex
     * @return the vertices adjacent to vertex {@code v}, as an iterable
     * @throws IllegalArgumentException unless {@code 0 <= v < V}
     */
    public Iterable<Integer> adj(int v) {
        return adj[v];
    }
}