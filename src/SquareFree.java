import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SquareFree {
    public static final int[] f4 = {0, 0, 1, 3, 5, 6, 7, 9, 11, 13, 16, 18, 21,
                                    24, 27, 30, 33};
    private boolean START_MAX_DEG = false;
    private boolean N4_PRED = true;
    private boolean SYMM_BREAK = false;
    private boolean LEX_SYMM_BREAK = false;
    private int n = 10;
    private int m = f4[n];

    // LEX 24 -- 143
    // PRE_DEG LEX 24 -- 144
    // FILE PRE_DEG LEX -- 41 UNSAT WTF
    // FILE LEX -- 45 UNSAT
    // REV_FILE LEX -- 42 UNSAT


    public SquareFree(String[] args) {

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
                case "--unsat":
                    m++;
                    break;
                case "--m":
                    m = Integer.parseInt(args[i + 1]);
                    i++;
                    break;
                default:
                    System.err.println("Unknown option: " + args[i]);
            }
        }

        //  n: 4, 5, 6, 7,  8,  9, 10, 11, 12, 13, 14, 15, 16
        //  f: 5, 6, 7, 9, 11, 13, 16, 18, 21, 24, 27, 30, 33
        // uf:                             23, 26, 28, 32, 35

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
        PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter("bee20160531/models/f4.bee")));

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
        if (SYMM_BREAK) {
            if (START_MAX_DEG) {
                // degree[0] == max_deg
                pw.println("int_eq(" + degree(0) + ", max_deg)");
            }

            // declaration of p[1..n-1]: 1..n
            for (int i = 1; i < n; i++) {
                pw.println("new_int(" + p(i) + ", 0, " + (i - 1) + ")");
            }

            // FORALL i, j. p[j] = i <=> (A[i, j] && !(EXISTS k < i. A[k, j]))
            for (int i = 0; i < n; i++) {
                for (int j = 1; j < n; j++) {
                    String X1 = nextBool(pw);
                    String X2 = nextBool(pw);

                    // p[j] = i <=> X1
                    pw.println("int_eq_reif(" + p(j) + ", " + i + ", " + X1 + ")");

                    // EXISTS k < i. A[k, j] <=> X2
                    List<String> list = new ArrayList<>();
                    for (int k = 0; k < i; k++) {
                        list.add(A(k, j));
                    }
                    pw.println("bool_array_or_reif(" + list + ", " + X2 + ")");

                    // A[i, j] && -X2 <=> X1
                    pw.println("bool_and_reif(" + A(i, j) + ", -" + X2 + ", " + X1 + ")");
                }
            }

            // p[i] <= p[i + 1]
            for (int i = 1; i < n - 1; i++) {
                pw.println("int_leq(" + p(i) + ", " + p(i + 1) + ")");
            }

            // declaration of w[0..n-1]: 1..n
            for (int i = 0; i < n; i++) {
                pw.println("new_int(" + w(i) + ", 1, " + (n - i) + ")");
            }

            // (p[i] == p[i + 1]) => (w[i] >= w[i + 1])
            for (int i = 1; i < n - 1; i++) {
                String X1 = nextBool(pw);
                String X2 = nextBool(pw);
                pw.println("int_eq_reif(" + p(i) + ", " + p(i + 1) + ", " + X1 + ")");
                pw.println("int_geq_reif(" + w(i) + ", " + w(i + 1) + ", " + X2 + ")");
                pw.println("bool_ite(" + X1 + ", " + X2 + ", true)");
            }

            // w[i] = 1 + sum(w[j] * bool2int(p[j] == i), j = i+1..n-1)
            for (int i = 0; i < n; i++) {
                List<String> list = new ArrayList<>();
                for (int j = i + 1; j < n; j++) {
                    String X1 = nextBool(pw);
                    String X2 = nextInt(pw, 0, 1);
                    String X3 = nextInt(pw, 0, n);
                    pw.println("int_eq_reif(" + p(j) + ", " + i + ", " + X1 + ")");
                    pw.println("bool2int(" + X1 + ", " + X2 + ")");
                    pw.println("int_times(" + w(j) + ", " + X2 + ", " + X3 + ")");
                    list.add(X3);
                }
                list.add("1");
                pw.println("int_array_sum_eq(" + list + ", " + w(i) + ")");
            }
        }

        if (LEX_SYMM_BREAK) {
            lexSymmBreak(pw, 0, n - 1);
        }

        pw.println("solve satisfy");
        pw.close();
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
