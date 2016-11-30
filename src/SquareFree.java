import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class SquareFree {        // 0  1  2  3  4  5  6  7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
    public static final int[] f4 = {0, 0, 1, 3, 5, 6, 7, 9, 11, 13, 16, 18, 21, 24, 27, 30, 33, 36, 39, 42, 46, 50, 52, 56, 59, 63, 67, 71, 76, 80, 85, 90};
                                         //  n: 5, 5, 6, 7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
                                         //  f: 5, 6, 7, 9, 11, 13, 16, 18, 21, 24, 27, 30, 33,(36)(39)(43)(46)
    private boolean N4_PRED = true;
    private boolean SYMM_BREAK = false;
    private boolean LEX_SYMM_BREAK = false;
    private boolean UNAVOIDABLE_SYMMBREAK = false;
    private boolean START_MAX_DEG = false;
    private boolean SORTED_WEIGHTS = false;
    private static final String PATH_TO_BEE = "/nfs/home/smoklev/BEECodeGen/bee20160830/";
    private int n = 10;
    private int m = f4[n];

    // LEX 24 -- 143
    // PRE_DEG LEX 24 -- 144
    // FILE PRE_DEG LEX -- 41 UNSAT WTF
    // FILE LEX -- 45 UNSAT
    // REV_FILE LEX -- 42 UNSAT


    public SquareFree(String[] args) {
        System.out.println(Arrays.toString(args));
        n = Integer.parseInt(args[0]);
        if (n >= 0 && n < f4.length)
            m = f4[n];
        else 
            m = 0;
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
                case "--start-max-deg":
                    START_MAX_DEG = true;
                    break;
                case "--sorted-weights":
                    SORTED_WEIGHTS = true;
                    break;
                case "--unavoid-symmbreak":
                    UNAVOIDABLE_SYMMBREAK = true;
                    break;
                case "--unsat":
                    m = f4[n] + 1;
                    break;
                case "--m":
                    m = Integer.parseInt(args[i + 1]);
                    i++;
                    break;
                default:
                    System.err.println("Unknown option: " + args[i]);
            }
        }

        //  n: 4, 5, 6, 7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        //  f: 5, 6, 7, 9, 11, 13, 16, 18, 21, 24, 27, 30, 33,(36)(39)(43)(46)
        // uf:                             23, 26, 28, 32, 35, 38, 41, 45, 48
        //n = 20;
        //m = 28;
        //SYMM_BREAK = true;
        //START_MAX_DEG = true;

        //n = Integer.parseInt(args[0]);
        //m = Integer.parseInt(args[1]);
        //SYMM_BREAK = true;
        //START_MAX_DEG = true;
    }

    public static void main(String[] args) throws IOException {
        try {
            if (args.length == 0)
                args = new String[] {"10"};
            new SquareFree(args).start();
        } catch (Exception e) {
            System.err.println("Usage: java SquareFree n [--n3|--symmbreak|--lex-symmbreak|--star max min]");
            e.printStackTrace();
        }
    }

    private void start() throws IOException {
        PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(PATH_TO_BEE + "models/f4.bee")));

        if (SYMM_BREAK && LEX_SYMM_BREAK) {
            throw new RuntimeException("Both symm breaks, may be unsat");
        }

        // n = 20 -- 26 s : SAT
        // n = 21 -- 459 s : SAT
        // n = 16 -- 234 s : UNSAT

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                pw.println("new_bool(" + A(i, j) + ")");
            }
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i < j) {
                    pw.println("bool_eq(" + A(i, j) + ", " + A(j, i) + ")");
                }
            }
        }

        for (int i = 0; i < n; i++) {
            pw.println("bool_eq(" + A(i, i) + ", false)");
        }

        if (N4_PRED) {
            // no 4-cycles, O(n^4) predicates
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    for (int k = 0; k < n; k++) {
                        for (int l = 0; l < n; l++) {
                            if (i != j && i != k && i != l && j != k && j != l && k != l) {
                                pw.println("bool_array_sum_lt([" + A(i, j) + ", " + A(j, k) + ", " + A(k, l) + ", " + A(l, i) + "], 4)");
                            }
                        }
                    }
                }
            }
        } else {
            // no 4-cycles, O(n^3) predicates
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    for (int k = 0; k < n; k++) {
                        if (i < k && i != j && k != j) {
                            pw.println("new_bool(" + x(i, j, k) + ")");
                            pw.println("bool_array_and_reif([" + A(i, j) + ", " + A(j, k) + "], " + x(i, j, k) + ")");
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
                                list.add(x(i, j, k));
                            }
                        }
                        pw.println("new_bool(" + x(i, k) + ")");
                        pw.println("bool_array_or_reif(" + list + ", " + x(i, k) + ")");
                    }
                }
            }

            for (int i = 0; i < n; i++) {
                for (int k = 0; k < n; k++) {
                    if (i < k) {
                        List<String> list = new ArrayList<>();
                        for (int j = 0; j < n; j++) {
                            if (j != i && j != k) {
                                list.add(x(i, j, k));
                            }
                        }
                        pw.println("bool_array_sum_lt(" + list + ", 2)");
                    }
                }
            }
        }

        pw.print("bool_array_sum_eq([");
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                pw.print(A(i, j));
                if (i != n - 1 || j != n - 1)
                    pw.print(", ");
            }
        }
        pw.println("], " + 2 * m + ")");

        // declaration of degree[]
        for (int i = 0; i < n; i++) {
            pw.println("new_int(" + degree(i) + ", 1, " + n + ")");
        }

        // declaration of min_deg, max_deg
        pw.println("new_int(min_deg, 1, " + n + ")");
        pw.println("new_int(max_deg, 1, " + n + ")");
        pw.println("int_leq(min_deg, max_deg)");                        // min_deg <= max_deg
        pw.println("int_geq(max_deg, " + ceilDiv(2 * m, n) + ")");      // max_deg >= 2 * m / n;

        // experimental constraints
            pw.println("new_int(min_deg_times_max_deg, 1, " + n * n + ")");
            pw.println("new_int(min_deg_m_1, 0, " + (n - 1) + ")");
            pw.println("int_plus(min_deg, -1, min_deg_m_1)");
            pw.println("int_times(min_deg_m_1, max_deg, min_deg_times_max_deg)");
            pw.println("int_leq(min_deg_times_max_deg, " + (n - 1) + ")");  // min_deg * max_deg <= n - 1
            pw.println("int_leq(min_deg, " + (int) Math.floor(0.5 * (1.0 + Math.sqrt(4.0 * n - 3.0))) + ")");
            //pw.println("int_geq(min_deg, " + (f4[n + 1] - f4[n]) + ")");
    

        for (int i = 0; i < n; i++) {
            // degree[i] = sum(A[i])
            pw.print("bool_array_sum_eq([");
            for (int j = 0; j < n; j++) {
                pw.print(A(i, j));
                if (j != n - 1)
                    pw.print(", ");
            }
            pw.println("], " + degree(i) + ")");

            // degree[i] <= max_deg
            pw.println("int_leq(" + degree(i) + ", max_deg)");

            // degree[i] >= min_deg
            pw.println("int_geq(" + degree(i) + ", min_deg)");
        }

        // min_deg = min(degree)
        pw.print("int_array_min([");
        for (int j = 0; j < n; j++) {
            pw.print(degree(j));
            if (j != n - 1)
                pw.print(", ");
        }
        pw.println("], min_deg)");

        // max_deg = max(degree)
        pw.print("int_array_max([");
        for (int j = 0; j < n; j++) {
            pw.print(degree(j));
            if (j != n - 1)
                pw.print(", ");
        }
        pw.println("], max_deg)");

        // ============= SYMMETRY BREAKING ==============
        if (UNAVOIDABLE_SYMMBREAK) {
            addUnavoidableSymmBreak(pw);
        }

        if (SYMM_BREAK) {
            addBFSSymmBreak(pw);
        }

        if (LEX_SYMM_BREAK) {
            lexSymmBreak(pw, 0, n - 1);
        }

        pw.println("solve satisfy");
        pw.close();
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

    private String var(String prefix, int... indices) {
        return prefix + Arrays.stream(indices).mapToObj(x -> "_" + x).collect(Collectors.joining());
    }

    private void lexSymmBreak(PrintWriter pw, int a, int b) {
        for (int i = a; i < b; i++) {
            for (int j = i + 1; j <= b; j++) {
                if (j - i != 2) {
                    List<String> listI = new ArrayList<>();
                    List<String> listJ = new ArrayList<>();
                    for (int k = 0; k < n; k++) {
                        if (k != i && k != j) {
                            listI.add(A(i, k));
                            listJ.add(A(j, k));
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

    private String w(int i) {
        return "w_" + i;
    }

    private String p(int i) {
        return "p_" + i;
    }

    private String A(int i, int j) {
        return "A_" + i + "_" + j;
    }

    private String degree(int i) {
        return "degree_" + i;
    }

    private String x(int i, int j, int k) {
        if (i >= k)
            throw new RuntimeException(i + ", " + j + ", " + k);
        return "x_" + i + "_" + j + "_" + k;
    }

    private String x(int i, int j) {
        if (i >= j)
            throw new RuntimeException(i + ", " + j);
        return "x_" + i + "_" + j;
    }

    private int ceilDiv(int n, int m) {
        return (int) Math.ceil((double) n / m);
    }
}
