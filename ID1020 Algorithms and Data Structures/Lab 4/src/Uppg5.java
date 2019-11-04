/*
* Result with database.txt and AL as start node
AL to AL:  AL
AL to FL:  AL-FL
AL to GA:  AL-GA
AL to MS:  AL-MS
AL to TN:  AL-TN
AL to AR:  not connected
AL to LA:  not connected
AL to MO:  not connected
AL to OK:  not connected
AL to TX:  not connected
AL to AZ:  not connected
AL to CA:  not connected
AL to NM:  not connected
AL to NV:  not connected
AL to UT:  not connected
AL to OR:  not connected
AL to CO:  not connected
AL to KS:  not connected
AL to NE:  not connected
AL to WY:  not connected
AL to CT:  not connected
AL to MA:  not connected
AL to NY:  not connected
AL to RI:  not connected
AL to DC:  not connected
AL to MD:  not connected
AL to VA:  AL-TN-VA
AL to DE:  not connected
AL to NJ:  not connected
AL to PA:  not connected
AL to NC:  AL-GA-NC
AL to SC:  AL-GA-SC
AL to IA:  not connected
AL to IL:  not connected
AL to MN:  not connected
AL to SD:  not connected
AL to WI:  not connected
AL to ID:  not connected
AL to MT:  not connected
AL to WA:  not connected
AL to IN:  not connected
AL to KY:  not connected
AL to MI:  not connected
AL to OH:  not connected
AL to WV:  AL-TN-VA-WV
AL to NH:  not connected
AL to VT:  not connected
AL to ME:  not connected
AL to ND:  not connected
* */


/**
 *  The class represents a data type for finding
 *  paths from a source vertex s to every other vertex
 *  in an undirected graph.
 *  This implementation uses depth-first search.
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Uppg5 {
    private boolean[] marked;    // marked[v] = is there an start-v path?
    private int[] edgeTo;        // edgeTo[v] = last edge on start-v path
    private final int start;         // source vertex

    /**
     * Computes a path between {@code start} and every other vertex in graph {@code G}.
     * @param G the graph
     * @param start the source vertex
     * @throws IllegalArgumentException unless {@code 0 <= start < V}
     */
    public Uppg5(Digraph G, int start) {
        this.start = start;
        edgeTo = new int[G.V()];
        marked = new boolean[G.V()];
        dfs(G, start);
    }

    // depth first search from v
    private void dfs(Digraph G, int v) {
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
        Digraph G = new Digraph(V, in);
        int s = Integer.parseInt(args[1]);
        Uppg5 dfs = new Uppg5(G, s);


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