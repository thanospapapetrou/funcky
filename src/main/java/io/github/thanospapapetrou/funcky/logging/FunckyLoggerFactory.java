package io.github.thanospapapetrou.funcky.logging;

import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

public class FunckyLoggerFactory { // TODO remove class
    public static Logger getLogger(final Class<?> clazz) {
        return Logger.getLogger(clazz.getName());
    }

    public static void configureLogger(final Class<?> clazz, final Level level) {
        final Logger logger = getLogger(clazz);
        logger.setLevel(level);
        logger.setUseParentHandlers(false);
        logger.addHandler(getHandler(level));
    }

    private static ConsoleHandler getHandler(final Level level) {
        final ConsoleHandler handler = new ConsoleHandler();
        handler.setLevel(level);
        handler.setFormatter(getFormatter());
        return handler;
    }

    private static FunckyLogFormatter getFormatter() {
        return new FunckyLogFormatter();
    }
}
