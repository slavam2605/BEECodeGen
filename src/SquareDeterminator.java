import java.io.*;

/**
 * @author Moklev Vyacheslav
 */
public class SquareDeterminator {//0  1  2  3  4  5  6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
    private static final int[] f_low  = {0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0};
    private static final int[] f_high = {0, 0, 1, 3, 5, 6, 8, 10, 12, 15, 17, 20, 23, 26, 28, 32, 35, 38, 41, 45, 48};
    private static final int NO_RESULT = -2;
    private static final int UNSAT = -1;
    private static final int UNKNOWN = 0;
    private static final int SAT = 1;

    private static int result = NO_RESULT;
    private static int count = 0;

    public static void main(String[] args) throws IOException, InterruptedException {
        PrintWriter pw = new PrintWriter(new FileWriter("SquareFreeSequence.txt"), true);
        for (int n = 5; n < 100; n++) {
            for (int m = f_low[n] + 1; m <= f_high[n]; m++) {
                Process p = Runtime.getRuntime().exec("your favourite command here");
                p.waitFor();
                BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
                reader.lines().filter(s -> s.startsWith("s")).forEach(s -> {
                    count++;
                    String ms = s.substring(1).trim();
                    if (ms.startsWith("UNK")) {
                        result = UNKNOWN;
                    } else if (ms.startsWith("SAT")) {
                        result = SAT;
                    } else if (ms.startsWith("UNSAT")) {
                        result = UNSAT;
                    }
                });
                pw.println(n + ", " + m + ": " + toString(result));
            }
        }
    }

    private static String toString(int result) {
        switch (result) {
            case NO_RESULT: return "NO_RESULT";
            case UNSAT: return "UNSAT";
            case UNKNOWN: return "UNKNOWN";
            case SAT: return "SAT";
            default: return "NO_SUCH_CASE(" + result + ")";
        }
    }
}
