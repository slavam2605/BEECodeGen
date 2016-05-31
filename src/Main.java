import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static final int[] f4 = {0, 0, 1, 2, 3, 5, 6, 8, 10, 12, 15, 16, 18, 21, 23, 26, 28, 31,
            34, 38, 41, 44, 47, 50, 54, 57, 61, 65, 68, 72, 76, 80, 85};

    public static void main(String[] args) throws IOException {
        PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter("../bee20160401/models/f4.bee")));

        // n = 20 -- 26 s : SAT
        // n = 21 -- 459 s : SAT
        // n = 16 -- ? s : UNSAT

        int n = 20;
        int m = f4[n];

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

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    if (i != j && j != k && i != k) {
                        pw.println("bool_array_sum_lt([" + A(i, j) + ", " + A(j, k) + ", " + A(k, i) + "], 3)");
                    }
                }
            }
        }

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

//        for (int i = 0; i < n; i++) {
//            for (int j = 0; j < n; j++) {
//                for (int k = 0; k < n; k++) {
//                    if (i < k && i != j && k != j) {
//                        pw.println("new_bool(" + x(i, j, k) + ")");
//                        pw.println("bool_array_and_reif([" + A(i, j) + ", " + A(j, k) + "], " + x(i, j, k) + ")");
//                    }
//                }
//            }
//        }
//
//        for (int i = 0; i < n; i++) {
//            for (int k = 0; k < n; k++) {
//                if (i < k) {
//                    List<String> list = new ArrayList<>();
//                    for (int j = 0; j < n; j++) {
//                        if (i != j && k != j) {
//                            list.add(x(i, j, k));
//                        }
//                    }
//                    pw.println("new_bool(" + x(i, k) + ")");
//                    pw.println("bool_array_or_reif(" + list + ", " + x(i, k) + ")");
//                }
//            }
//        }
//
//        for (int i = 0; i < n; i++) {
//            for (int k = 0; k < n; k++) {
//                if (i < k) {
//                    /*pw.println("bool_and_reif(" + A(i, k) + ", " + x(i, k) + ", false)");*/
//                    pw.println("bool_array_sum_lt([" + A(i, k) + ", " + x(i, k) + "], 2)");
//                }
//            }
//        }
//
//        for (int i = 0; i < n; i++) {
//            for (int k = 0; k < n; k++) {
//                if (i < k) {
//                    List<String> list = new ArrayList<>();
//                    for (int j = 0; j < n; j++) {
//                        if (j != i && j != k) {
//                            list.add(x(i, j, k));
//                        }
//                    }
//                    pw.println("bool_array_sum_lt(" + list + ", 2)");
//                }
//            }
//        }

        for (int i = 0; i < n; i++) {
            pw.println("new_int(" + degree(i) + ", 1, " + n + ")");
        }

        pw.println("new_int(min_deg, 1, " + n + ")");
        pw.println("new_int(max_deg, 1, " + n + ")");

        // pre-calculated values
//        pw.println("new_int(min_deg, 3, 3)");
//        pw.println("new_int(max_deg, 5, 5)");

        pw.println("new_int(min_deg_times_max_deg, 1, " + n * n + ")");
        pw.println("int_times(min_deg, max_deg, min_deg_times_max_deg)");
        pw.println("int_leq(min_deg_times_max_deg, " + (n - 1) + ")");  // min_deg * max_deg <= n - 1
        pw.println("int_leq(min_deg, max_deg)");                        // min_deg <= max_deg
        pw.println("int_geq(min_deg, " + (m - f4[n - 1]) + ")");        // min_deg >= m - f4[n - 1];
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

        pw.print("bool_array_sum_eq([");
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                pw.print(A(i, j));
                if (i != n - 1 || j != n - 1)
                    pw.print(", ");
            }
        }
        pw.println("], " + 2 * m + ")");

        pw.println("solve satisfy");
        pw.close();
    }

    private static String A(int i, int j) {
        return "A_" + i + "_" +  j;
    }

    private static String degree(int i) {
        return "degree_" + i;
    }

    private static String x(int i, int j, int k) {
        if (i >= k)
            throw new RuntimeException(i + ", " + j + ", " + k);
        return "x_" + i + "_" +  j + "_" + k;
    }

    private static String x(int i, int j) {
        if (i >= j)
            throw new RuntimeException(i + ", " + j);
        return "x_" + i + "_" +  j;
    }

    private static int ceilDiv(int n, int m) {
        return (int) Math.ceil((double) n / m);
    }
}
