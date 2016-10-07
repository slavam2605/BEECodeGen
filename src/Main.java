import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Main {
    public static final int[] f4 = {0, 0, 1, 2, 3, 5, 6, 8, 10, 12, 15, 16, 18, 21, 23, 26, 28, 31,
            34, 38, 41, 44, 47, 50, 54, 57, 61, 65, 68, 72, 76, 80, 85};
    private static final String PATH_TO_BEE = "C:\\Users\\Home\\Downloads\\bee20160830\\";
    private boolean N4_PRED = true;
    private boolean SYMM_BREAK = false;
    private boolean LEX_SYMM_BREAK = false;
    private boolean PRE_DEG = false;
    private boolean START_MAX_DEG = false;
    private boolean SORTED_WEIGHTS = false;
    private boolean LOAD_A = false;
    private boolean UNAVOIDABLE_SYMMBREAK = false;
    private String a_file_name = "5-3-star.txt";
    private int pre_max_deg = 5;
    private int pre_min_deg = 4;
    private int n = 10;
    private int m = f4[n];

    // LEX 24 -- 143
    // PRE_DEG LEX 24 -- 144
    // FILE PRE_DEG LEX -- 41 UNSAT WTF
    // FILE LEX -- 45 UNSAT
    // REV_FILE LEX -- 42 UNSAT


    public Main(String[] args) {
        n = Integer.parseInt(args[0]);
        m = f4[n];
        for (int i = 1; i < args.length; i++) {
            switch (args[i]) {
                case "--n3":
                    N4_PRED = false;
                    break;
                case "--symmbreak":
                    SYMM_BREAK = true;
                    break;
                case "--lex-symmbreak":
                    LEX_SYMM_BREAK = true;
                    break;
                case "--unavoid-symmbreak":
                    UNAVOIDABLE_SYMMBREAK = true;
                    break;
                case "--star":
                    LOAD_A = true;
                    PRE_DEG = true;
                    int max_deg = Integer.parseInt(args[i + 1]);
                    int min_deg = Integer.parseInt(args[i + 2]);
                    i += 2;
                    a_file_name = max_deg + "-" + (min_deg - 1) + "-star.txt";
                    pre_max_deg = max_deg;
                    pre_min_deg = min_deg;
                    break;
                case "--start-max-deg":
                    START_MAX_DEG = true;
                    break;
                case "--unsat":
                    m = f4[n] + 1;
                    break;
                case "--sorted-weights":
                    SORTED_WEIGHTS = true;
                    break;
                default:
                    System.err.println("Unknown option: " + args[i]);
            }
        }
    }

    public static void main(String[] args) throws IOException {
        try {
            new Main(args).start();
        } catch (Exception e) {
            System.err.println("Usage: java Main n [--n3|--symmbreak|--lex-symmbreak|" +
                    "--star max min|--start-max-deg|--sorted-weights|--unavoid-symmbreak|--unsat]");
            e.printStackTrace();
        }
    }

    private void start() throws IOException {
        System.out.println("n = " + n + ", m = " + m + ", SYMM_BREAK = " + SYMM_BREAK + ", LEX_SYMM_BREAK = " + LEX_SYMM_BREAK + ", START_MAX_DEG = " +
            START_MAX_DEG + ", N4_PRED = " + N4_PRED);
        PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(PATH_TO_BEE + "models" + File.separatorChar + "f4.bee")));

        if (SYMM_BREAK && LEX_SYMM_BREAK) {
            throw new RuntimeException("Both symm breaks, may be unsat");
        }

        if (LOAD_A && !PRE_DEG) {
            System.err.println("LOAD_A without PRE_DEG looks suspicious");
        }

        // n = 20 -- 26 s : SAT
        // n = 21 -- 459 s : SAT
        // n = 16 -- 234 s : UNSAT

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                pw.println("new_bool(" + var("A", i, j) + ")");
            }
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i < j) {
                    pw.println("bool_eq(" + var("A", i, j) + ", " + var("A", j, i) + ")");
                }
            }
        }

        for (int i = 0; i < n; i++) {
            pw.println("bool_eq(" + var("A", i, i) + ", false)");
        }

        if (LOAD_A) {
            loadAdjacencyMatrix(pw);
        }

        if (N4_PRED) {
            addBisquareConstraints(pw);
        } else {
            addCubicConstraints(pw);
        }

        // declaration of degree[]
        for (int i = 0; i < n; i++) {
            pw.println("new_int(" + var("degree", i) + ", 1, " + n + ")");
        }

        // declaration of min_deg, max_deg
        if (PRE_DEG) {
            pw.println("new_int(min_deg, " + pre_min_deg + ", " + pre_min_deg + ")");
            pw.println("new_int(max_deg, " + pre_max_deg + ", " + pre_max_deg + ")");
        } else {
            pw.println("new_int(min_deg, 1, " + n + ")");
            pw.println("new_int(max_deg, 1, " + n + ")");
            pw.println("new_int(min_deg_times_max_deg, 1, " + n * n + ")");
            pw.println("int_times(min_deg, max_deg, min_deg_times_max_deg)");
            //pw.println("int_leq(min_deg_times_max_deg, " + (n - 1) + ")");  // min_deg * max_deg <= n - 1
            //pw.println("int_leq(min_deg, max_deg)");                        // min_deg <= max_deg
            //pw.println("int_geq(min_deg, " + (m - f4[n - 1]) + ")");        // min_deg >= m - f4[n - 1];
            //pw.println("int_geq(max_deg, " + ceilDiv(2 * m, n) + ")");      // max_deg >= 2 * m / n;
        }

        for (int i = 0; i < n; i++) {
            // degree[i] = sum(A[i])
            pw.print("bool_array_sum_eq([");
            for (int j = 0; j < n; j++) {
                pw.print(var("A", i, j));
                if (j != n - 1)
                    pw.print(", ");
            }
            pw.println("], " + var("degree", i) + ")");

            // degree[i] <= max_deg
            pw.println("int_leq(" + var("degree", i) + ", max_deg)");

            // degree[i] >= min_deg
            pw.println("int_geq(" + var("degree", i) + ", min_deg)");
        }

        // min_deg = min(degree)
        pw.print("int_array_min([");
        for (int j = 0; j < n; j++) {
            pw.print(var("degree", j));
            if (j != n - 1)
                pw.print(", ");
        }
        pw.println("], min_deg)");

        // max_deg = max(degree)
        pw.print("int_array_max([");
        for (int j = 0; j < n; j++) {
            pw.print(var("degree", j));
            if (j != n - 1)
                pw.print(", ");
        }
        pw.println("], max_deg)");

        pw.print("bool_array_sum_eq([");
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                pw.print(var("A", i, j));
                if (i != n - 1 || j != n - 1)
                    pw.print(", ");
            }
        }
        pw.println("], " + 2 * m + ")");

        // ============= SYMMETRY BREAKING ==============

        if (UNAVOIDABLE_SYMMBREAK) {
            addUnavoidableSymmBreak(pw);
        }

        if (SYMM_BREAK) {
            addBFSSymmBreak(pw);
        }

        if (LEX_SYMM_BREAK) {
            addLexSymmBreak(pw);
        }

        pw.println("solve satisfy");
        pw.close();
    }

    private void addLexSymmBreak(PrintWriter pw) {
        if (LOAD_A) {
            if (n == 24) {
                lexSymmBreak(pw, 6, 8);
                lexSymmBreak(pw, 9, 11);
                lexSymmBreak(pw, 12, 14);
                lexSymmBreak(pw, 15, 17);
                lexSymmBreak(pw, 18, 20);

                lexSymmBreak(pw, 21, 23);
            } else if (n == 19) {
                lexSymmBreak(pw, 5, 7);
                lexSymmBreak(pw, 8, 10);
                lexSymmBreak(pw, 11, 13);
                lexSymmBreak(pw, 14, 16);

                lexSymmBreak(pw, 17, 18);
            } else {
                throw new RuntimeException("Unable to make partial lex-symmbreak with star and n = " + n);
            }
        } else {
            lexSymmBreak(pw, 0, n - 1);
        }
    }

    private void addBFSSymmBreak(PrintWriter pw) {
        addBFSConstraint(pw);
        if (START_MAX_DEG) {
            addStartMaxDegConstraint(pw);
        }
        if (SORTED_WEIGHTS) {
            addSortedWeightsConstraint(pw);
        }
    }

    private void addStartMaxDegConstraint(PrintWriter pw) {
        // degree[0] == max_deg
        pw.println("int_eq(" + var("degree", 0) + ", max_deg)");
    }

    private void addSortedWeightsConstraint(PrintWriter pw) {
        // declaration of w[0..n-1]: 1..n
        for (int i = 0; i < n; i++) {
            pw.println("new_int(" + var("w", i) + ", 1, " + (n - i) + ")");
        }

        // (p[i] == p[i + 1]) => (w[i] >= w[i + 1])
        for (int i = 1; i < n - 1; i++) {
            String X1 = nextBool(pw);
            String X2 = nextBool(pw);
            pw.println("int_eq_reif(" + var("p", i) + ", " + var("p", i + 1) + ", " + X1 + ")");
            pw.println("int_geq_reif(" + var("w", i) + ", " + var("w", i + 1) + ", " + X2 + ")");
            pw.println("bool_ite(" + X1 + ", " + X2 + ", true)");
        }

        // w[i] = 1 + sum(w[j] * bool2int(p[j] == i), j = i+1..n-1)
        for (int i = 0; i < n; i++) {
            List<String> list = new ArrayList<>();
            for (int j = i + 1; j < n; j++) {
                String X1 = nextBool(pw);
                String X2 = nextInt(pw, 0, 1);
                String X3 = nextInt(pw, 0, n);
                pw.println("int_eq_reif(" + var("p", j) + ", " + i + ", " + X1 + ")");
                pw.println("bool2int(" + X1 + ", " + X2 + ")");
                pw.println("int_times(" + var("w", j) + ", " + X2 + ", " + X3 + ")");
                list.add(X3);
            }
            list.add("1");
            pw.println("int_array_sum_eq(" + list + ", " + var("w", i) + ")");
        }
    }

    private void addBFSConstraint(PrintWriter pw) {
        // declaration of p[1..n-1]: 1..n
        for (int i = 1; i < n; i++) {
            pw.println("new_int(" + var("p", i) + ", 0, " + (i - 1) + ")");
        }

        // FORALL i, j. p[j] = i <=> (A[i, j] && !(EXISTS k < i. A[k, j]))
        for (int i = 0; i < n; i++) {
            for (int j = 1; j < n; j++) {
                String X1 = nextBool(pw);
                String X2 = nextBool(pw);

                // p[j] = i <=> X1
                pw.println("int_eq_reif(" + var("p", j) + ", " + i + ", " + X1 + ")");

                // EXISTS k < i. A[k, j] <=> X2
                List<String> list = new ArrayList<>();
                for (int k = 0; k < i; k++) {
                    list.add(var("A", k, j));
                }
                pw.println("bool_array_or_reif(" + list + ", " + X2 + ")");

                // A[i, j] && -X2 <=> X1
                pw.println("bool_and_reif(" + var("A", i, j) + ", -" + X2 + ", " + X1 + ")");
            }
        }

        // p[i] <= p[i + 1]
        for (int i = 1; i < n - 1; i++) {
            pw.println("int_leq(" + var("p", i) + ", " + var("p", i + 1) + ")");
        }
    }

    private void addUnavoidableSymmBreak(PrintWriter pw) {
        pw.println("bool_eq(" + var("A", 0, 3) + ", " + var("A", 0, 4) + ")");
        pw.println("bool_eq(" + var("A", 0, 4) + ", " + var("A", 0, 5) + ")");
        pw.println("bool_eq(" + var("A", 0, 5) + ", " + var("A", 1, 3) + ")");
        pw.println("bool_eq(" + var("A", 1, 3) + ", " + var("A", 1, 4) + ")");
        pw.println("bool_eq(" + var("A", 1, 4) + ", " + var("A", 1, 5) + ")");
        pw.println("bool_eq(" + var("A", 1, 5) + ", " + var("A", 1, 6) + ")");
        pw.println("bool_eq(" + var("A", 1, 6) + ", " + var("A", 2, 4) + ")");
        pw.println("bool_eq(" + var("A", 2, 4) + ", " + var("A", 2, 5) + ")");
        pw.println("bool_eq(" + var("A", 2, 5) + ", " + var("A", 2, 6) + ")");
        pw.println("bool_eq(" + var("A", 2, 6) + ", " + var("A", 2, 7) + ")");
        pw.println("bool_eq(" + var("A", 2, 7) + ", " + var("A", 2, 8) + ")");
    }

    private void addCubicConstraints(PrintWriter pw) {
        // no 3- and 4-cycles, O(n^3) predicates
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    if (i < k && i != j && k != j) {
                        pw.println("new_bool(" + var("x", i, j, k) + ")");
                        pw.println("bool_and_reif(" + var("A", i, j) + ", " + var("A", j, k) + ", " + var("x", i, j, k) + ")");
                    }
                }
            }
        }

        for (int i = 0; i < n; i++) {
            for (int k = 0; k < n; k++) {
                if (i < k) {
                    List<String> list = new ArrayList<>();
                    for (int j = 0; j < n; j++) {
                        if (i != j && k != j) {
                            list.add(var("x", i, j, k));
                        }
                    }
                    pw.println("new_bool(" + var("x", i, k) + ")");
                    pw.println("bool_array_or_reif(" + list + ", " + var("x", i, k) + ")");
                }
            }
        }

        for (int i = 0; i < n; i++) {
            for (int k = 0; k < n; k++) {
                if (i < k) {
                    pw.println("bool_and_reif(" + var("A", i, k) + ", " + var("x", i, k) + ", false)");
                   // pw.println("bool_array_sum_lt([" + A(i, k) + ", " + x(i, k) + "], 2)");
                }
            }
        }

        for (int i = 0; i < n; i++) {
            for (int k = 0; k < n; k++) {
                if (i < k) {
                    List<String> list = new ArrayList<>();
                    for (int j = 0; j < n; j++) {
                        if (j != i && j != k) {
                            list.add(var("x", i, j, k));
                        }
                    }
                    pw.println("bool_array_sum_lt(" + list + ", 2)");
                }
            }
        }
    }

    private void addBisquareConstraints(PrintWriter pw) {
        // no 3- and 4-cycles, O(n^4) predicates
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    if (i != j && j != k && i != k) {
                        //pw.println("bool_array_sum_lt([" + A(i, j) + ", " + A(j, k) + ", " + A(k, i) + "], 3)");
                        pw.println("bool_array_or([-" + var("A", i, j) + ", -" + var("A", j, k) + ", -" + var("A", k, i) + "])");
                    }
                }
            }
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    for (int l = 0; l < n; l++) {
                        if (i != j && i != k && i != l && j != k && j != l && k != l) {
                           //  pw.println("bool_array_sum_lt([" + A(i, j) + ", " + A(j, k) + ", " + A(k, l) + ", " + A(l, i) + "], 4)");
                            pw.println("bool_array_or([-" + var("A", i, j) + ", -" + var("A", j, k) + ", -" + var("A", k, l) + ", -" + var("A", l, i) + "])");
                        }
                    }
                }
            }
        }
    }

    private void loadAdjacencyMatrix(PrintWriter pw) throws IOException {
        BufferedReader bf = new BufferedReader(new FileReader(a_file_name));
        for (int i = 0; i < n; i++) {
            String s = bf.readLine();
            for (int j = 0; j < n; j++) {
                switch (s.charAt(j)) {
                    case '0':
                        pw.println("bool_eq(" + var("A", i, j) + ", false)");
                        break;
                    case '1':
                        pw.println("bool_eq(" + var("A", i, j) + ", true)");
                        break;
                }
            }
        }
    }

    private void lexSymmBreak(PrintWriter pw, int a, int b) {
        for (int i = a; i < b; i++) {
            for (int j = i + 1; j <= b; j++) {
                if (j - i != 2) {
                    List<String> listI = new ArrayList<>();
                    List<String> listJ = new ArrayList<>();
                    for (int k = 0; k < n; k++) {
                        if (k != i && k != j) {
                            listI.add(var("A", i, k));
                            listJ.add(var("A", j, k));
                        }
                    }
                    pw.println("bool_arrays_lex(" + listI + ", " + listJ + ")");
                }
            }
        }
    }

    private int lastTemp = 0;
    private String nextBool(PrintWriter pw) {
        pw.println("new_bool(temp_" + lastTemp + ")");
        return "temp_" + lastTemp++;
    }

    private String nextInt(PrintWriter pw, int a, int b) {
        pw.println("new_int(temp_" + lastTemp + ", " + a + ", " + b + ")");
        return "temp_" + lastTemp++;
    }

    private String var(String prefix, int... indices) {
        return prefix + Arrays.stream(indices).mapToObj(x -> "_" + x).collect(Collectors.joining());
    }

    private int ceilDiv(int n, int m) {
        return (int) Math.ceil((double) n / m);
    }
}
