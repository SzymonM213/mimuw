package przyklady03;

import java.util.function.IntBinaryOperator;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.Semaphore;

public class MatrixRowSums {

    private static final int ROWS = 10;
    private static final int COLUMNS = 100;

    private static class Matrix {

        private final int rows;
        private final int columns;
        private final IntBinaryOperator definition;
        private static final CyclicBarrier barrier = new CyclicBarrier(COLUMNS);
        private static final Semaphore mutex = new Semaphore(1);
        public Matrix(int rows, int columns, IntBinaryOperator definition) {
            this.rows = rows;
            this.columns = columns;
            this.definition = definition;
        }

        public static class Column implements Runnable {
            
            private final int columnId;
            private final int rowSums[];
            private final IntBinaryOperator definition;
            private final CyclicBarrier barrier;

            public Column(int columnId, int rowSums[], IntBinaryOperator definition, CyclicBarrier barrier) {
                this.columnId = columnId;
                this.rowSums = rowSums;
                this.definition = definition;
                this.barrier = barrier;
            }

            @Override
            public void run() {
                for (int i = 0; i < ROWS; i++) {
                    int tmp = definition.applyAsInt(i, this.columnId);
                    try {
                        mutex.acquire();
                    } catch (InterruptedException e) {}
                    this.rowSums[i] += tmp;
                    mutex.release();
                    // try {
                    //     barrier.await();
                    // } catch (InterruptedException | BrokenBarrierException e) {}
                }
            }
        }

        public int[] rowSums() {
            int[] rowSums = new int[rows];
            for (int row = 0; row < rows; ++row) {
                int sum = 0;
                for (int column = 0; column < columns; ++column) {
                    sum += definition.applyAsInt(row, column);
                }
                rowSums[row] = sum;
            }
            return rowSums;
        }

        public int[] rowSumsConcurrent() {
            int[] rowSums = new int[rows];
            Thread[] columns = new Thread[COLUMNS];
            for (int i = 0; i < COLUMNS; i++) {
                columns[i] = new Thread(new Column(i, rowSums, definition, barrier));
            }
            for (int i = 0; i < COLUMNS; i++) {
                columns[i].start();
            }
            try {
                for (int i = 0; i < COLUMNS; i++) {
                    columns[i].join();
            }
            } catch (InterruptedException e) {
                Thread t = Thread.currentThread();
                t.interrupt();
                System.err.println(t.getName() + " interrupted");
            }
            return rowSums;
        }
    }

    public static void main(String[] args) {
        Matrix matrix = new Matrix(ROWS, COLUMNS, (row, column) -> {
            int a = 2 * column + 1;
            int cellId = column + row * COLUMNS;
            try {
                Thread.sleep((1000 - (cellId % 13) * 1000 / 12));
            } catch (InterruptedException e) {
                Thread t = Thread.currentThread();
                t.interrupt();
                System.err.println(t.getName() + " interrupted");
            }
            return (row + 1) * (a % 4 - 2) * a;
        });
        // long startTime = System.currentTimeMillis();
        // int[] rowSums = matrix.rowSums();
        // long usedTime = System.currentTimeMillis() - startTime;
        // System.out.println("Sequential execution took: " + usedTime + "ms");
        // System.out.println("Result:");
        // for (int i = 0; i < rowSums.length; i++) {
        //     System.out.println(i + " -> " + rowSums[i]);
        // }
        
        
        // concurrent computations
        long startTime = System.currentTimeMillis();
        int[] rowSums = matrix.rowSumsConcurrent();
        long usedTime = System.currentTimeMillis() - startTime;
        System.out.println("Concurrent execution took: " + usedTime + "ms");
        System.out.println("Result:");
        for (int i = 0; i < rowSums.length; i++) {
            System.out.println(i + " -> " + rowSums[i]);
        }
    }

}
