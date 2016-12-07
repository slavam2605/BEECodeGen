import javafx.util.Pair;

import java.io.*;
import java.nio.file.Files;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ResultParser {

    public static final int MAX_N = 40;

    public static class TimeElapsed {
        int realTime;
        int userTime;
        int sysTime;
        int mode = 0; // 0 -- unknown, 1 -- time, 2 -- tl

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
            Pattern tl = Pattern.compile("TL: ([0-9]*)h");
            Pattern tlMin = Pattern.compile("TL: ([0-9]*)m"); 
            TimeElapsed time = new TimeElapsed();
            BufferedReader br = new BufferedReader(new FileReader(file));
            br.lines().forEach(s -> {
                Matcher realMatcher = real.matcher(s);
                Matcher userMatcher = user.matcher(s);
                Matcher sysMatcher = sys.matcher(s);
                Matcher tlMatcher = tl.matcher(s);
                Matcher tlMinMatcher = tlMin.matcher(s);
                if (realMatcher.find()) {
                    time.realTime = Integer.parseInt(realMatcher.group(1)) * 60
                            + Integer.parseInt(realMatcher.group(2));
                    time.mode = 1;
                } else if (userMatcher.find()) {
                    time.userTime = Integer.parseInt(userMatcher.group(1)) * 60
                            + Integer.parseInt(userMatcher.group(2));
                    time.mode = 1;
                } else if (sysMatcher.find()) {
                    time.sysTime = Integer.parseInt(sysMatcher.group(1)) * 60
                            + Integer.parseInt(sysMatcher.group(2));
                    time.mode = 1;
                } else if (tlMatcher.find()) {
                    time.realTime = Integer.parseInt(tlMatcher.group(1)) * 3600;
                    time.mode = 2;
                } else if (tlMinMatcher.find()) {
                    time.realTime = Integer.parseInt(tlMinMatcher.group(1)) * 60;
                    time.mode = 2;
                }
            });
            return time;
        } catch (FileNotFoundException e) {
            return null;
        }
    }

    private static Set<String> getSpecifiers() throws IOException {
        Set<String> specifiers = new HashSet<>();
        Pattern p = Pattern.compile("err(.*)_([0-9]+).*");
        Files.list(new File("/nfs/home/smoklev/tests/").toPath())
            .filter(path -> p.matcher(path.getFileName().toString()).find())
            .forEach(path -> {
                Matcher m = p.matcher(path.getFileName().toString());
                m.find();
                specifiers.add(m.group(1));
            });
        return specifiers;
    }

    private static List<Pair<File, FileParams>> getFiles(String specifier) throws IOException {
        Pattern errPattern = Pattern.compile("err" + specifier + "_([0-9]+)([0-9a-z\\- ]*)");
        return Files.list(new File("/nfs/home/smoklev/tests/").toPath())
                .filter(path -> errPattern.matcher(path.getFileName().toString()).find())
                .filter(path -> !path.getFileName().toString().endsWith("temp"))
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
        Map<String, TimeElapsed[]> map = new LinkedHashMap<>();
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
                    System.out.println(i + "\t\t" + TLR(times[i]) + "\t\t" + TLUS(times[i]));
                }
            }
        });
        System.out.println("====================== Symm break comparison ======================");
        List<String> ids = Arrays.asList("", "--n3", "--unsat", "--unsat --n3");
        for (String id: ids) {
            System.out.println("========================== " + (id.isEmpty() ? "(no flags)" : id) + " ==========================");
            System.out.println(" \t\t\t\treal\t\t\t\t\t\t(user+sys)");
            System.out.println("n\t\tbaseline\tsymmbreak+\tsymmbreak*\tlex-symmbreak\tbaseline\tsymmbreak+\tsymmbreak*\tlex-symmbreak");
            TimeElapsed[] times0 = map.get(id);
            TimeElapsed[] timesBFS = map.get(id + String.join(" ", Arrays.asList("--symmbreak")).trim()); 
            TimeElapsed[] timesPLUS = map.get(id + String.join(" ", Arrays.asList("--symmbreak", "--start-max-deg")).trim());
            TimeElapsed[] timesSTAR = map.get(id + String.join(" ", Arrays.asList("--symmbreak", "--start-max-deg", "--sorted-weights")).trim());
            TimeElapsed[] timesUNAVOID = map.get(id + String.join(" ", Arrays.asList("--unavoid-symmbreak")).trim());
            TimeElapsed[] timesLEX = map.get(id + String.join(" ", Arrays.asList("--lex-symmbreak")).trim());
            System.out.println(times0 + ", " + timesBFS + ", " + timesPLUS + ", " + timesSTAR + ", " + timesUNAVOID + ", " + timesLEX);
            if (times0 == null || timesBFS == null || timesPLUS == null || timesSTAR == null || timesUNAVOID == null || timesLEX == null)
                continue;
            for (int i = 0; i < MAX_N; i++) {
                if (times0[i] != null || timesBFS[i] != null || timesPLUS[i] != null ||
                    timesSTAR[i] != null || timesUNAVOID[i] != null || timesLEX[i] != null) {
                    System.out.print(i + " & ");
                    System.out.printf("%s & %s & %s & %s & %s & %s",  TLR(times0[i]), 
                                     TLR(timesBFS[i]),
                                     TLR(timesPLUS[i]),
                                     TLR(timesSTAR[i]),
                                     TLR(timesUNAVOID[i]),
                                     TLR(timesLEX[i]));
                    System.out.println(" \\\\");
                }
            }
        }
    }

    static String TLR(TimeElapsed time) {
        if (time == null)
            return "    NULL";
        switch (time.mode) {
            case 0: 
                return "     UNK";
            case 1: 
                return String.format("% 8d", time.realTime);
            case 2: 
                 return String.format("% 7d", time.realTime) + "+";
                //return "++++";
            default: throw new RuntimeException("Unknown mode: 2");
        }
    }

    static String TLUS(TimeElapsed time) {
        switch (time.mode) {
            case 0: 
                return "UNK";
            case 1: 
                return "" + (time.userTime + time.sysTime) / 32;
            case 2: 
                return time.realTime + "+";
            default: throw new RuntimeException("Unknown mode: 2");
        }
    }

    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.out.println("Usage: java ResultParser [specifier]");
            System.out.println("Available specifiers: " + getSpecifiers());
            return;
        }
        List<Pair<FileParams, TimeElapsed>> list = getFiles(args[0]).stream()
                .map(ffp -> new Pair<>(ffp.getValue(), readFile(ffp.getKey())))
                .collect(Collectors.toList());
        nicePrint(list);
    }

}
