import javafx.util.Pair;

import java.io.*;
import java.nio.file.Files;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author Моклев Вячеслав
 */
public class ResultParser {

    public static final int MAX_N = 30;

    private static class TimeElapsed {
        int realTime = 0;
        int userTime = 0;
        int sysTime = 0;

        @Override
        public String toString() {
            return "TimeElapsed{" +
                    "realTime=" + realTime +
                    ", userTime=" + userTime +
                    ", sysTime=" + sysTime +
                    '}';
        }
    }

    private static class FileParams {
        int n;
        String[] flags;

        @Override
        public String toString() {
            return "FileParams{" +
                    "n=" + n +
                    ", flags=" + Arrays.toString(flags) +
                    '}';
        }
    }

    private static TimeElapsed readFile(File file) {
        try {
            Pattern real = Pattern.compile("real[ \t]*([0-9]+)m([0-9]+)\\.[0-9]+s");
            Pattern user = Pattern.compile("user[ \t]*([0-9]+)m([0-9]+)\\.[0-9]+s");
            Pattern sys = Pattern.compile("sys[ \t]*([0-9]+)m([0-9]+)\\.[0-9]+s");
            TimeElapsed time = new TimeElapsed();
            BufferedReader br = new BufferedReader(new FileReader(file));
            br.lines().forEach(s -> {
                Matcher realMatcher = real.matcher(s);
                Matcher userMatcher = user.matcher(s);
                Matcher sysMatcher = sys.matcher(s);
                if (realMatcher.find()) {
                    time.realTime += Integer.parseInt(realMatcher.group(1)) * 60
                            + Integer.parseInt(realMatcher.group(2));
                } else if (userMatcher.find()) {
                    time.userTime += Integer.parseInt(userMatcher.group(1)) * 60
                            + Integer.parseInt(userMatcher.group(2));
                } else if (sysMatcher.find()) {
                    time.sysTime += Integer.parseInt(sysMatcher.group(1)) * 60
                            + Integer.parseInt(sysMatcher.group(2));
                }
            });
            return time;
        } catch (FileNotFoundException e) {
            return null;
        }
    }

    private static List<Pair<File, FileParams>> getFiles() throws IOException {
        Pattern errPattern = Pattern.compile("err_([0-9]+)([0-9a-z\\- ]*)");
        return Files.list(new File("").toPath())
                .filter(path -> errPattern.matcher(path.getFileName().toString()).find())
                .map(path -> {
                    String fileName = path.getFileName().toString();
                    Matcher matcher = errPattern.matcher(fileName);
                    if (matcher.find()) {
                        FileParams params = new FileParams();
                        params.n = Integer.parseInt(matcher.group(1));
                        params.flags = matcher.group(2).split(" ");
                        return new Pair<>(path.toFile(), params);
                    }
                    return null;
                }).collect(Collectors.toList());
    }

    private static void nicePrint(List<Pair<FileParams, TimeElapsed>> list) {
        Map<String, TimeElapsed[]> map = new HashMap<>();
        for (Pair<FileParams, TimeElapsed> fpt : list) {
            String id = String.join(" ", fpt.getKey().flags);
            if (!map.containsKey(id))
                map.put(id, new TimeElapsed[MAX_N]);
            TimeElapsed[] times = map.get(id);
            times[fpt.getKey().n] = fpt.getValue();
        }
        map.forEach((s, times) -> {
            System.out.println("====== " + (s.isEmpty() ? "(no flags)" : s) + " ======");
            System.out.println("n\t\treal\t\t(user+sys)/2");
            for (int i = 0; i < MAX_N; i++) {
                if (times[i] != null) {
                    System.out.println(i + "\t\t" + times[i].realTime + "\t\t" + (times[i].userTime + times[i].sysTime) / 2 / 32);
                }
            }
        });
        System.out.println("====================== Symm break comparison ======================");
        for (String id = ""; id.length() < 6; id += "--n3 ") {
            System.out.println("========================== " + (id.isEmpty() ? "(no flags)" : id) + " ==========================");
            System.out.println(" \t\t\t\treal\t\t\t\t\t\t(user+sys/2)");
            System.out.println("n\t\tsymmbreak\tlex-symmbreak\tsymmbreak\tlex-symmbreak");
            TimeElapsed[] times1 = map.get(id + "--symmbreak");
            TimeElapsed[] times2 = map.get(id + "--lex-symmbreak");
            if (times1 == null || times2 == null)
                continue;
            for (int i = 0; i < MAX_N; i++) {
                if (times1[i] != null && times2[i] != null) {
                    System.out.print(i + "\t\t");
                    System.out.print(times1[i].realTime + "\t\t" + times2[i].realTime + "\t\t\t");
                    System.out.print((times1[i].userTime + times1[i].sysTime) / 2 / 32 + "\t\t\t" + (times2[i].userTime + times2[i].sysTime) / 2 / 32);
                    System.out.println();
                }
            }
        }
    }

    public static void main(String[] args) throws IOException {
        List<Pair<FileParams, TimeElapsed>> list = getFiles().stream()
                .map(ffp -> new Pair<>(ffp.getValue(), readFile(ffp.getKey())))
                .collect(Collectors.toList());
        nicePrint(list);
    }

}
