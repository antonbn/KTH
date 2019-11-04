/*
* Result with database.txt and AL as start node
AL to AL:  AL
AL to FL:  AL-TN-VA-NC-SC-GA-FL
AL to GA:  AL-TN-VA-NC-SC-GA
AL to MS:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-AR-MS
AL to TN:  AL-TN
AL to AR:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-AR
AL to LA:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-AR-MS-LA
AL to MO:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO
AL to OK:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK
AL to TX:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX
AL to AZ:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ
AL to CA:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA
AL to NM:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM
AL to NV:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV
AL to UT:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT
AL to OR:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR
AL to CO:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-NE-KS-CO
AL to KS:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-NE-KS
AL to NE:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-NE
AL to WY:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY
AL to CT:  AL-TN-VA-WV-PA-NY-VT-NH-MA-RI-CT
AL to MA:  AL-TN-VA-WV-PA-NY-VT-NH-MA
AL to NY:  AL-TN-VA-WV-PA-NY
AL to RI:  AL-TN-VA-WV-PA-NY-VT-NH-MA-RI
AL to DC:  AL-TN-VA-WV-PA-NY-NJ-DE-MD-DC
AL to MD:  AL-TN-VA-WV-PA-NY-NJ-DE-MD
AL to VA:  AL-TN-VA
AL to DE:  AL-TN-VA-WV-PA-NY-NJ-DE
AL to NJ:  AL-TN-VA-WV-PA-NY-NJ
AL to PA:  AL-TN-VA-WV-PA
AL to NC:  AL-TN-VA-NC
AL to SC:  AL-TN-VA-NC-SC
AL to IA:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-NE-IA
AL to IL:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-NE-IA-IL
AL to MN:  AL-TN-VA-WV-PA-OH-MI-WI-MN
AL to SD:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD
AL to WI:  AL-TN-VA-WV-PA-OH-MI-WI
AL to ID:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-WA-ID
AL to MT:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-WA-ID-MT
AL to WA:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-WA
AL to IN:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-NE-IA-IL-KY-IN
AL to KY:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-CA-AZ-NM-TX-OK-MO-NE-IA-IL-KY
AL to MI:  AL-TN-VA-WV-PA-OH-MI
AL to OH:  AL-TN-VA-WV-PA-OH
AL to WV:  AL-TN-VA-WV
AL to NH:  AL-TN-VA-WV-PA-NY-VT-NH
AL to VT:  AL-TN-VA-WV-PA-NY-VT
AL to ME:  AL-TN-VA-WV-PA-NY-VT-NH-ME
AL to ND:  AL-TN-VA-WV-PA-OH-MI-WI-MN-SD-WY-UT-NV-OR-WA-ID-MT-ND
* */

/**
 *  The class represents a data type for finding
 *  paths from a source vertex <em>s</em> to every other vertex
 *  in an undirected graph.
 *  <p>
 *  This implementation uses depth-first search.
 *  The constructor takes time proportional to <em>V</em> + <em>E</em>,
 *  where <em>V</em> is the number of vertices and <em>E</em> is the number of edges.
 *  Each call to {@link #hasPathTo(int)} takes constant time;
 *  each call to {@link #pathTo(int)} takes time proportional to the length
 *  of the path.
 *  It uses extra space (not including the graph) proportional to <em>V</em>.
 *  <p>
 *  For additional documentation, see <a href="https://algs4.cs.princeton.edu/41graph">Section 4.1</a>
 *  of <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
 *
 *  @author Robert Sedgewick
 *  @author Kevin Wayne
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Uppg1 {
    private boolean[] marked;    // marked[v] = is there an start-v path?
    private int[] edgeTo;        // edgeTo[v] = last edge on start-v path
    private final int start;         // source vertex

    /**
     * Computes a path between {@code start} and every other vertex in graph {@code G}.
     * @param G the graph
     * @param start the source vertex
     * @throws IllegalArgumentException unless {@code 0 <= start < V}
     */
    public Uppg1(Graph G, int start) {
        this.start = start;
        edgeTo = new int[G.V()];
        marked = new boolean[G.V()];
        dfs(G, start);
    }

    // depth first search from v
    private void dfs(Graph G, int v) {
        marked[v] = true;
        for (int w : G.adj(v)) {
            if (!marked[w]) {
                edgeTo[w] = v;
                dfs(G, w);
            }
        }
    }

    /**
     * Is there a path between the source vertex {@code start} and vertex {@code v}?
     * @param v the vertex
     * @return {@code true} if there is a path, {@code false} otherwise
     * @throws IllegalArgumentException unless {@code 0 <= v < V}
     */
    public boolean hasPathTo(int v) {
        return marked[v];
    }

    /**
     * Returns a path between the source vertex {@code start} and vertex {@code v}, or
     * {@code null} if no such path.
     * @param  v the vertex
     * @return the sequence of vertices on a path between the source vertex
     *         {@code start} and vertex {@code v}, as an Iterable
     * @throws IllegalArgumentException unless {@code 0 <= v < V}
     */
    public Iterable<Integer> pathTo(int v) {
        if (!hasPathTo(v)) return null;
        Stack<Integer> path = new Stack<>();
        for (int x = v; x != start; x = edgeTo[x]) {
            path.push(x);
        }
        path.push(start);
        return path;
    }

    /**
     * Unit tests the {@code Uppg1} data type.
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
        Uppg1 dfs = new Uppg1(G, s);

        for (int v = 0; v < G.V(); v++) {
            if (dfs.hasPathTo(v)) {
                System.out.printf("%s to %s:  ", G.getConversion(s), G.getConversion(v));
                for (int x : dfs.pathTo(v)) {
                    if (x == s) System.out.print(G.getConversion(x));
                    else        System.out.print("-" + G.getConversion(x));
                }
                System.out.println();
            }

            else {
                System.out.printf("%s to %s:  not connected\n", G.getConversion(s), G.getConversion(v));
            }

        }
    }

}