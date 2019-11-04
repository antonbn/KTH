/*
* Result with database.txt and AL as start node
AL to AL (0):  AL
AL to FL (1):  AL-FL
AL to GA (1):  AL-GA
AL to MS (1):  AL-MS
AL to TN (1):  AL-TN
AL to AR (2):  AL-TN-AR
AL to LA (2):  AL-MS-LA
AL to MO (2):  AL-TN-MO
AL to OK (3):  AL-TN-MO-OK
AL to TX (3):  AL-TN-AR-TX
AL to AZ (5):  AL-TN-MO-OK-NM-AZ
AL to CA (6):  AL-TN-MO-OK-NM-AZ-CA
AL to NM (4):  AL-TN-MO-OK-NM
AL to NV (6):  AL-TN-MO-OK-NM-AZ-NV
AL to UT (5):  AL-TN-MO-OK-CO-UT
AL to OR (6):  AL-TN-MO-NE-WY-ID-OR
AL to CO (4):  AL-TN-MO-OK-CO
AL to KS (3):  AL-TN-MO-KS
AL to NE (3):  AL-TN-MO-NE
AL to WY (4):  AL-TN-MO-NE-WY
AL to CT (6):  AL-TN-VA-WV-PA-NY-CT
AL to MA (6):  AL-TN-VA-WV-PA-NY-MA
AL to NY (5):  AL-TN-VA-WV-PA-NY
AL to RI (7):  AL-TN-VA-WV-PA-NY-MA-RI
AL to DC (3):  AL-TN-VA-DC
AL to MD (3):  AL-TN-VA-MD
AL to VA (2):  AL-TN-VA
AL to DE (4):  AL-TN-VA-MD-DE
AL to NJ (5):  AL-TN-VA-WV-PA-NJ
AL to PA (4):  AL-TN-VA-WV-PA
AL to NC (2):  AL-TN-NC
AL to SC (2):  AL-GA-SC
AL to IA (3):  AL-TN-MO-IA
AL to IL (3):  AL-TN-MO-IL
AL to MN (4):  AL-TN-MO-IA-MN
AL to SD (4):  AL-TN-MO-NE-SD
AL to WI (4):  AL-TN-MO-IL-WI
AL to ID (5):  AL-TN-MO-NE-WY-ID
AL to MT (5):  AL-TN-MO-NE-WY-MT
AL to WA (6):  AL-TN-MO-NE-WY-ID-WA
AL to IN (3):  AL-TN-KY-IN
AL to KY (2):  AL-TN-KY
AL to MI (4):  AL-TN-KY-OH-MI
AL to OH (3):  AL-TN-KY-OH
AL to WV (3):  AL-TN-VA-WV
AL to NH (7):  AL-TN-VA-WV-PA-NY-VT-NH
AL to VT (6):  AL-TN-VA-WV-PA-NY-VT
AL to ME (8):  AL-TN-VA-WV-PA-NY-VT-NH-ME
AL to ND (5):  AL-TN-MO-NE-SD-ND
* */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

/**
 *  The class represents a data type for finding
 *  shortest paths (number of edges) from a source vertex <em>s</em>
 *  to every other vertex in an undirected graph.
 *  <p>
 *  This implementation uses breadth-first search.
 *  The constructor takes time proportional to <em>V</em> + <em>E</em>,
 *  where <em>V</em> is the number of vertices and <em>E</em> is the number of edges.
 *  Each call to {@link #distTo(int)} and {@link #hasPathTo(int)} takes constant time;
 *  each call to {@link #pathTo(int)} takes time proportional to the length
 *  of the path.
 *  It uses extra space (not including the graph) proportional to <em>V</em>.
 *  <p>
 *  For additional documentation,
 *  see <a href="https://algs4.cs.princeton.edu/41graph">Section 4.1</a>
 *  of <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */
public class Uppg2 {
    private static final int INFINITY = Integer.MAX_VALUE;
    private boolean[] marked;  // marked[v] = is there an s-v path
    private int[] edgeTo;      // edgeTo[v] = previous edge on shortest s-v path
    private int[] distTo;      // distTo[v] = number of edges shortest s-v path

    /**
     * Computes the shortest path between the source vertex {@code s}
     * and every other vertex in the graph {@code G}.
     * @param G the graph
     * @param s the source vertex
     * @throws IllegalArgumentException unless {@code 0 <= s < V}
     */
    public Uppg2(Graph G, int s) {
        marked = new boolean[G.V()];
        distTo = new int[G.V()];
        edgeTo = new int[G.V()];
        bfs(G, s);
    }

    // breadth-first search from a single source
    private void bfs(Graph G, int s) {
        Queue<Integer> q = new Queue<>();
        for (int v = 0; v < G.V(); v++)
            distTo[v] = INFINITY;
        distTo[s] = 0;
        marked[s] = true;
        q.enqueue(s);

        while (!q.isEmpty()) {
            int v = q.dequeue();
            for (int w : G.adj(v)) {
                if (!marked[w]) {
                    edgeTo[w] = v;
                    distTo[w] = distTo[v] + 1;
                    marked[w] = true;
                    q.enqueue(w);
                }
            }
        }
    }

    /**
     * Is there a path between the source vertex {@code s} (or sources) and vertex {@code v}?
     * @param v the vertex
     * @return {@code true} if there is a path, and {@code false} otherwise
     * @throws IllegalArgumentException unless {@code 0 <= v < V}
     */
    public boolean hasPathTo(int v) {
        return marked[v];
    }

    /**
     * Returns the number of edges in a shortest path between the source vertex {@code s}
     * (or sources) and vertex {@code v}?
     * @param v the vertex
     * @return the number of edges in a shortest path
     * @throws IllegalArgumentException unless {@code 0 <= v < V}
     */
    public int distTo(int v) {
        return distTo[v];
    }

    /**
     * Returns a shortest path between the source vertex {@code s} (or sources)
     * and {@code v}, or {@code null} if no such path.
     * @param  v the vertex
     * @return the sequence of vertices on a shortest path, as an Iterable
     * @throws IllegalArgumentException unless {@code 0 <= v < V}
     */
    public Iterable<Integer> pathTo(int v) {
        if (!hasPathTo(v)) return null;
        Stack<Integer> path = new Stack<Integer>();
        int x;
        for (x = v; distTo[x] != 0; x = edgeTo[x])
            path.push(x);
        path.push(x);
        return path;
    }

    /**
     * Unit tests the {@code Uppg2} data type.
     *
     * @param args the command-line arguments
     */
    public static void main(String[] args) throws FileNotFoundException {
        // Takes the file specified as input
        File file = new File(args[0]);
        Scanner in = new Scanner(file);

        Stack<String> stack = new Stack<>();
        int V = 0;
        while (in.hasNext()) {
            String key = in.next();
            if (!stack.contains(key)) {
                stack.push(key);
                V++;
            }
        }

        in = new Scanner(file);
        Graph G = new Graph(V, in);
        int s = Integer.parseInt(args[1]);
        Uppg2 bfs = new Uppg2(G, s);

        for (int v = 0; v < G.V(); v++) {
            if (bfs.hasPathTo(v)) {
                System.out.printf("%s to %s (%d):  ", G.getConversion(s), G.getConversion(v), bfs.distTo(v));
                for (int x : bfs.pathTo(v)) {
                    if (x == s) System.out.print(G.getConversion(x));
                    else        System.out.print("-" + G.getConversion(x));
                }
                System.out.println();
            }

            else {
                System.out.printf("%s to %s (-):  not connected\n", G.getConversion(s), G.getConversion(v));
            }

        }
    }


}