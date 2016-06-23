import java.io.*;
import java.util.concurrent.TimeUnit;

/**
 * @author Moklev Vyacheslav
 */
public class SquareDeterminator {     // 0  1  2  3  4  5  6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
    private static final int[] f_low  = {0, 0, 1, 2, 3, 4, 5,  6,  8, 11, 13, 16, 19, 22, 24, 28, 31, 36, 39, 42, 46};
    private static final int[] f_high = {0, 0, 1, 3, 5, 6, 8, 10, 12, 15, 17, 20, 23, 26, 28, 32, 35, 38, 41, 45, 48};
    private static final int NO_RESULT = -2;
    private static final int UNSAT = -1;
    private static final int UNKNOWN = 0;
    private static final int SAT = 1;

    private static int result = NO_RESULT;
    private static int count = 0;

    public static void main(String[] args) throws IOException, InterruptedException {
        for (int n = 5; n <= 16; n++) {
            boolean encUnsat = false;
            for (int m = f_low[n] + 1; m <= f_high[n]; m++) {
                Process p = Runtime.getRuntime().exec("/nfs/home/smoklev/tests/launchSq.sh " + n + " --m " + m + " --symmbreak --start-max-deg");
                boolean flag = p.waitFor(1, TimeUnit.HOURS);
                if (!flag) {
                    p.destroyForcibly();
                    result = UNKNOWN;
                    System.out.println(n + ", " + m + ": " + toString(result));
                    break;
                }
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
                System.out.println(n + ", " + m + ": " + toString(result));
                if (result == UNSAT) {
                    encUnsat = true;
                    break;
                }
            }
            if (!encUnsat) {
                System.out.println(n + ", " + (f_high[n] + 1) + ": UNSAT (upper bound)");
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
