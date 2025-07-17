package io.github.thanospapapetrou.funcky.logging;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;

public class FunckyLogFormatter extends SimpleFormatter {
    private static final Map<Level, AnsiColor> COLORS = Map.of(
            Level.SEVERE, AnsiColor.RED,
            Level.WARNING, AnsiColor.YELLOW,
            Level.INFO, AnsiColor.WHITE,
            Level.CONFIG, AnsiColor.GREEN,
            Level.FINE, AnsiColor.BLUE,
            Level.FINER, AnsiColor.PURPLE,
            Level.FINEST, AnsiColor.CYAN
    );
    private static final String FORMAT = "%1$s%2$s%3$s%n";

    @Override
    public String format(final LogRecord record) {
        return String.format(FORMAT, COLORS.get(record.getLevel()), record.getMessage(), AnsiColor.RESET);
    }
}
