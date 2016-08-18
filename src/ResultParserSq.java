import javafx.util.Pair;

import java.io.*;
import java.nio.file.Files;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ResultParserSq {

    public static final int MAX_N = 40;

    private static class TimeElapsed {
        int realTime = -100;
        int userTime = -100;
        int sysTime = -100;

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
                    time.realTime = Integer.parseInt(realMatcher.group(1)) * 60
                            + Integer.parseInt(realMatcher.group(2));
                } else if (userMatcher.find()) {
                    time.userTime = Integer.parseInt(userMatcher.group(1)) * 60
                            + Integer.parseInt(userMatcher.group(2));
                } else if (sysMatcher.find()) {
                    time.sysTime = Integer.parseInt(sysMatcher.group(1)) * 60
                            + Integer.parseInt(sysMatcher.group(2));
                }
            });
            return time;
        } catch (FileNotFoundException e) {
            return null;
        }
    }

    private static List<Pair<File, FileParams>> getFiles() throws IOException {
        Pattern errPattern = Pattern.compile("errSq_([0-9]+)([0-9a-z\\- ]*)");
        return Files.list(new File("/nfs/home/smoklev/tests/").toPath())
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
            System.out.println("n\t\treal\t\t(user+sys)");
            for (int i = 0; i < MAX_N; i++) {
                if (times[i] != null) {
                    System.out.println(i + "\t\t" + TL(times[i].realTime) + "\t\t" + TL((times[i].userTime + times[i].sysTime) / 32));
                }
            }
        });
        System.out.println("====================== Symm break comparison ======================");
        List<String> ids = Arrays.asList("", "--n3", "--unsat", "--unsat --n3");
        for (String id: ids) {
            System.out.println("========================== " + (id.isEmpty() ? "(no flags)" : id) + " ==========================");
            System.out.println(" \t\t\t\treal\t\t\t\t\t\t(user+sys)");
            System.out.println("n\t\tbaseline\tsymmbreak\tlex-symmbreak\tbaseline\tsymmbreak\tlex-symmbreak");
    
            TimeElapsed[] times0 = map.get(id);
            TimeElapsed[] times05 = map.get(String.join(" ", Arrays.asList(id, "--symmbreak")).trim());
            TimeElapsed[] times1 = map.get(String.join(" ", Arrays.asList(id, "--symmbreak", "--start-max-deg")).trim());
            TimeElapsed[] times2 = map.get(String.join(" ", Arrays.asList(id, "--lex-symmbreak")).trim());
            System.out.println(times0 + ", " + times05 + ", " + times1 + ", " + times2);
            if (times1 == null || times2 == null || times0 == null/* || times05 == null*/)
                continue;
            for (int i = 0; i < MAX_N; i++) {
                if (times1[i] != null && times2[i] != null) {
                    System.out.print(i + " & ");
                    if (i <= 8)
                    System.out.print(TL(times0[i].realTime) + " & " + /* TL(times05[i].realTime) + " & " +*/ TL(times1[i].realTime) + " & " + TL(times2[i].realTime));
                    if (i > 8)
                    System.out.print(TL((times0[i].userTime + times0[i].sysTime)/ 32) + " & " + /*TL((times05[i].userTime + times05[i].sysTime) / 32) + " & "
                                    + */TL((times1[i].userTime + times1[i].sysTime) / 32) + " & " + TL((times2[i].userTime + times2[i].sysTime) / 32));
                    System.out.println(" \\\\");
                }
            }
        }
    }

    private static String TL(int x) {
        if (x < 0)
            return "3600+";
        else
            return "" + x;
    }

    public static void main(String[] args) throws IOException {
        List<Pair<FileParams, TimeElapsed>> list = getFiles().stream()
                .map(ffp -> new Pair<>(ffp.getValue(), readFile(ffp.getKey())))
                .collect(Collectors.toList());
        nicePrint(list);
    }

}
