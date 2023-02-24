package przyklady04;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.IntBinaryOperator;

public class MatrixRowSums2Solution {

    private static class Matrix {

        private final int rows;
        private final int columns;
        private final IntBinaryOperator definition;

        private static final Map<Integer, AtomicInteger> sums = new ConcurrentHashMap<>();
        private static final Map<Integer, AtomicInteger> columnCnt = new ConcurrentHashMap<>();

        public Matrix(int rows, int columns, IntBinaryOperator definition) {
            this.rows = rows;
            this.columns = columns;
            this.definition = definition;
        }

        public void rowSums(TimedPrinter t) {
            for (int row = 0; row < rows; ++row) {
                int sum = 0;
                for (int column = 0; column < columns; ++column) {
                    sum += definition.applyAsInt(row, column);
                }
                t.println(String.format("row %d sum: %d", row, sum));
            }
        }
        
        public static class Column implements Runnable {
            
            private final int columnId;
            private final IntBinaryOperator definition;
            private final int rows;
            private final int columns;

            private final TimedPrinter p;

            public Column(int columnId, IntBinaryOperator definition, int rows, int columns, TimedPrinter p) {
                this.columnId = columnId;
                this.definition = definition;
                this.rows = rows;
                this.columns = columns;
                this.p = p;
            }

            @Override
            public void run() {
                try {
                    for (int i = 0; i < this.rows; i++) {
                        if (Thread.currentThread().isInterrupted()) {
                            throw new InterruptedException();
                        }
                        sums.putIfAbsent(i, new AtomicInteger(0));
                        columnCnt.putIfAbsent(i, new AtomicInteger(0));
                        sums.get(i).addAndGet(definition.applyAsInt(i, this.columnId));
                        columnCnt.get(i).incrementAndGet();
                        if (columnCnt.get(i).get() == this.columns) {
                            int tmp = sums.remove(i).get();
                            columnCnt.remove(i);
                            p.println(String.format("row %d sum: %d", i, tmp));
                        }
                    }
                } catch (InterruptedException e) {
                    Thread t = Thread.currentThread();
                    t.interrupt();
                }
            }
        }

        public void rowSumsConcurrent(TimedPrinter p) {
            Thread[] columnThreads = new Thread[this.columns];
            for (int i = 0; i < this.columns; i++) {
                columnThreads[i] = new Thread(new Column(i, definition, rows, columns, p));
            }
            for (int i = 0; i < columns; i++) {
                columnThreads[i].start();
            }
            for (int i = 0; i < columns; i++) {
                try {
                    columnThreads[i].join();
                } catch (InterruptedException e) {
                    for (Thread t: columnThreads) {
                        t.interrupt();
                    }
                    break;
                }
            }
                
                // Thread t = Thread.currentThread();
                // t.interrupt();
                // System.err.println(t.getName() + " interrupted");
        }
    }

    public static class TimedPrinter{
        final long startTime;
        public TimedPrinter() {
            startTime = System.currentTimeMillis();
        }
        synchronized void println(String s) {
            long elapsedTime = System.currentTimeMillis() - startTime;
            System.out.print(String.format("[%2.2fs] ", elapsedTime / 1000.));
            System.out.println(s);
        }

    }

    public static void main(String[] args) {
        // correctness test -----------------------------------------------------------------------
        Matrix matrix = new Matrix(3, 10, (row, column) -> {
            // long computations
            int a = 2 * column + 1;
            int cellId = column + row * 10;
            try {
                // different cells computations in rows takes different time to complete
                // and hence some thread will wait for others
                // but nevertheless there should be substantial gain from concurrent solutions
                Thread.sleep((1000 - (cellId % 13) * 1000 / 12));
            } catch (InterruptedException e) {
                Thread t = Thread.currentThread();
                t.interrupt();
                System.err.println(t.getName() + " interrupted");
            }
            return (row + 1) * (a % 4 - 2) * a;
        });
        // Uncomment to run sequential execution
        /*
        System.out.println("Running sequential execution (should take about 17s to complete)...");
        TimedPrinter timedPrinter = new TimedPrinter();
        timedPrinter.println("start computations");
        matrix.rowSums(timedPrinter);
        timedPrinter.println("end computations");
        */

        // concurrent computations
        System.out.println("Running concurrent execution"
            + "(should take about 2s to complete)...");
        TimedPrinter timedPrinter2 = new TimedPrinter();
        timedPrinter2.println("start computations");
        matrix.rowSumsConcurrent(timedPrinter2);
        timedPrinter2.println("end computations");

        // interrupt test -----------------------------------------------------------------------
        Matrix bigMatrix = new Matrix(2000000000, 10, (row, column) -> {
            // long computations
            int a = 2 * column + 1;
            int cellId = column + row * 10;
            try {
                Thread.sleep((1000 - (cellId % 13) * 1000 / 12));
            } catch (InterruptedException e) {
                Thread t = Thread.currentThread();
                t.interrupt();
                System.err.println(t.getName() + " interrupted");
            }
            return (row + 1) * (a % 4 - 2) * a;
        });
        System.out.println("Running long computations which would require millenia to complete...");
        System.out.println("(shouldn't result in memory error)");
        TimedPrinter timedPrinter3 = new TimedPrinter();
        Thread computations = new Thread(() -> {
            bigMatrix.rowSumsConcurrent(timedPrinter3);
        });
        timedPrinter3.println("start computations");
        int initialNumOfThreads = Thread.activeCount();
        computations.start();
        timedPrinter3.println("waiting 5s and then interrupting computations");
        try {
            Thread.sleep(5000);
        timedPrinter3.println("interrupting computations");
        computations.interrupt();
        timedPrinter3.println("waiting for computations to end");
        computations.join();
        for(;;) {
            int activeThreadsNum = Thread.activeCount();
            if (activeThreadsNum <= initialNumOfThreads) {
                timedPrinter3.println("everything done");
                break;
            } else {
                timedPrinter3.println(String.format("%d threads running, expected %d! checking again in 1s", activeThreadsNum, initialNumOfThreads));
                Thread.sleep(1000);
            }
        }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            System.err.println("Main interrupted");
        }
    }
}
