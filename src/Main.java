import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main {
    public static final int[] f4 = {0, 0, 1, 2, 3, 5, 6, 8, 10, 12, 15, 16, 18, 21, 23, 26, 28, 31,
            34, 38, 41, 44, 47, 50, 54, 57, 61, 65, 68, 72, 76, 80, 85};
    private boolean N4_PRED = true;
    private boolean SYMM_BREAK = false;
    private boolean LEX_SYMM_BREAK = false;
    private boolean PRE_DEG = false;
    private boolean LOAD_A = false;
    private String a_file_name = "5-3-star.txt";
    private int pre_max_deg = 5;
    private int pre_min_deg = 4;
    private int n = 5;
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
                default:
                    System.err.println("Unknown option: " + args[i]);
            }
        }
    }

    public static void main(String[] args) throws IOException {
        try {
            new Main(args).start();
        } catch (Exception e) {
            System.err.println("Usage: java Main n [--n3|--symmbreak|--lex-symmbreak|--star max min]");
            e.printStackTrace();
        }
    }

    private void start() throws IOException {
        PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter("bee20160531/models/f4.bee")));

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

        if (LOAD_A) {
            BufferedReader bf = new BufferedReader(new FileReader(a_file_name));
            for (int i = 0; i < n; i++) {
                String s = bf.readLine();
                for (int j = 0; j < n; j++) {
                    switch (s.charAt(j)) {
                        case '0':
                            pw.println("bool_eq(" + A(i, j) + ", false)");
                            break;
                        case '1':
                            pw.println("bool_eq(" + A(i, j) + ", true)");
                            break;
                    }
                }
            }
        }

        if (N4_PRED) {
            // no 3- and 4-cycles, O(n^4) predicates
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    for (int k = 0; k < n; k++) {
                        if (i != j && j != k && i != k) {
                            //pw.println("bool_array_sum_lt([" + A(i, j) + ", " + A(j, k) + ", " + A(k, i) + "], 3)");
                            pw.println("bool_array_or([-" + A(i, j) + ", -" + A(j, k) + ", -" + A(k, i) + "])");
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
                                pw.println("bool_array_or([-" + A(i, j) + ", -" + A(j, k) + ", -" + A(k, l) + ", -" + A(l, i) + "])");
                            }
                        }
                    }
                }
            }
        } else {
            // no 3- and 4-cycles, O(n^3) predicates
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    for (int k = 0; k < n; k++) {
                        if (i < k && i != j && k != j) {
                            pw.println("new_bool(" + x(i, j, k) + ")");
                            pw.println("bool_and_reif(" + A(i, j) + ", " + A(j, k) + ", " + x(i, j, k) + ")");
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
                    pw.println("bool_and_reif(" + A(i, k) + ", " + x(i, k) + ", false)");
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
                                list.add(x(i, j, k));
                            }
                        }
                        pw.println("bool_array_sum_lt(" + list + ", 2)");
                    }
                }
            }
        }

        // declaration of degree[]
        for (int i = 0; i < n; i++) {
            pw.println("new_int(" + degree(i) + ", 1, " + n + ")");
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
            pw.println("int_leq(min_deg_times_max_deg, " + (n - 1) + ")");  // min_deg * max_deg <= n - 1
            pw.println("int_leq(min_deg, max_deg)");                        // min_deg <= max_deg
            pw.println("int_geq(min_deg, " + (m - f4[n - 1]) + ")");        // min_deg >= m - f4[n - 1];
            pw.println("int_geq(max_deg, " + ceilDiv(2 * m, n) + ")");      // max_deg >= 2 * m / n;
        }

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

        pw.print("bool_array_sum_eq([");
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                pw.print(A(i, j));
                if (i != n - 1 || j != n - 1)
                    pw.print(", ");
            }
        }
        pw.println("], " + 2 * m + ")");

        // ============= SYMMETRY BREAKING ==============

        if (SYMM_BREAK) {
            // degree[0] == max_deg
//            pw.println("int_eq(" + degree(0) + ", max_deg)");

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
