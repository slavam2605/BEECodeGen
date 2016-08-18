import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Counter {
    private boolean SYMM_BREAK = false;
    private boolean LEX_SYMM_BREAK = false;
    private boolean START_MAX_DEG = false;
    private int n = 10;


    public Counter(String[] args) {
        n = Integer.parseInt(args[0]);
        for (int i = 1; i < args.length; i++) {
            switch (args[i]) {
                case "--symmbreak":
                    SYMM_BREAK = true;
                    break;
                case "--lex-symmbreak":
                    LEX_SYMM_BREAK = true;
                    break;
                case "--start-max-deg":
                    START_MAX_DEG = true;
                    break;
                default:
                    System.err.println("Unknown option: " + args[i]);
            }
        }
    }

    public static void main(String[] args) throws IOException {
        try {
            new Counter(args).start();
        } catch (Exception e) {
            System.err.println("Usage: java Counter n [--symmbreak|--lex-symmbreak|--start-max-deg]");
            e.printStackTrace();
        }
    }

    private void start() throws IOException {
        PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter("../bee20160531/models/f4.bee")));

        if (SYMM_BREAK && LEX_SYMM_BREAK) {
            throw new RuntimeException("Both symm breaks, may be unsat");
        }

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


        /*

        // declaration of degree[]
        for (int i = 0; i < n; i++) {
            pw.println("new_int(" + degree(i) + ", 0, " + n + ")");
        }


        // declaration of min_deg, max_deg
        pw.println("new_int(min_deg, 0, " + n + ")");
        pw.println("new_int(max_deg, 0, " + n + ")");
        pw.println("int_leq(min_deg, max_deg)");                        // min_deg <= max_deg

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

        */

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
