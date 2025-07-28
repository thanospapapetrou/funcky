package io.github.thanospapapetrou.funcky.logging;

import java.time.Duration;
import java.util.Map;
import java.util.stream.Collectors;

public class DurationFormatter {
    private static final String DURATION_DELIMITER = " ";
    private static final String DURATION_HOURS = "%1$d h";
    private static final String DURATION_MILLIS = "%1$d ms";
    private static final String DURATION_MINUTES = "%1$d m";
    private static final String DURATION_SECONDS = "%1$d s";

    public static String format(final Duration duration) {
        final long hours = duration.toHours();
        final long minutes = duration.minus(Duration.ofHours(hours)).toMinutes();
        final long seconds = duration.minus(Duration.ofHours(hours)).minus(Duration.ofMinutes(minutes)).toSeconds();
        final long millis = duration.minus(Duration.ofHours(hours)).minus(Duration.ofMinutes(minutes))
                .minus(Duration.ofSeconds(seconds)).toMillis();
        final String result = Map.of(
                        DURATION_HOURS, hours,
                        DURATION_MINUTES, minutes,
                        DURATION_SECONDS, seconds,
                        DURATION_MILLIS, millis
                ).entrySet().stream()
                .filter(d -> d.getValue() > 0)
                .map(d -> String.format(d.getKey(), d.getValue()))
                .collect(Collectors.joining(DURATION_DELIMITER));
        return result.isEmpty() ? String.format(DURATION_MILLIS, 0L) : result;
    }
}
