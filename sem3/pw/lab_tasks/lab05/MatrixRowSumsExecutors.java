package przyklady05;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.IntBinaryOperator;

import javax.swing.plaf.basic.BasicComboPopup.InvocationKeyHandler;

public class MatrixRowSumsExecutors {

    private static final int ROWS = 3;
    private static final int COLUMNS = 10;

    private static class Matrix {

        private final int rows;
        private final int columns;
        private final IntBinaryOperator definition;

        public Matrix(int rows, int columns, IntBinaryOperator definition) {
            this.rows = rows;
            this.columns = columns;
            this.definition = definition;
        }

        private static class Counting implements Callable<Integer> {

            private final int row;
            private final int column;
            private final IntBinaryOperator definition;
    
            private Counting(int row, int column, IntBinaryOperator definition) {
                this.row = row;
                this.column = column;
                this.definition = definition;
            }
    
            @Override
            public Integer call() throws InterruptedException {
                return this.definition.applyAsInt(row, column);
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
            int res[] = new int[ROWS];
            // YOUR CODE GOES HERE
            ExecutorService Pool = Executors.newFixedThreadPool(4);
            List<List<Future<Integer>>> matrix = new ArrayList<>();
            try {
                for (int i = 0; i < ROWS; i++) {
                    List<Callable<Integer>> calculations = new ArrayList<>();
                    for (int j = 0; j < COLUMNS; j++) {
                        Callable<Integer> work = new Counting(i, j, definition);
                        calculations.add(work);
                    }
                    matrix.add(Pool.invokeAll(calculations));
                }
                for (int i = 0; i < ROWS; i++) {
                    for (var v: matrix.get(i)) {
                        res[i] += v.get();
                    }
                }
            } catch (ExecutionException | InterruptedException e) {
                Thread t = Thread.currentThread();
                t.interrupt();
                System.err.println(t.getName() + " interrupted");
            } finally {
                Pool.shutdown();
            }
            // YOUR CODE GOES HERE
            return res;
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

        long startTime = System.currentTimeMillis();
        long usedTime = System.currentTimeMillis() - startTime;
        //int[] rowSums = matrix.rowSums();
        // System.out.println("Sequential execution took: " + usedTime + "ms");
        // System.out.println("Result:");
        // for (int i = 0; i < rowSums.length; i++) {
        //     System.out.println(i + " -> " + rowSums[i]);
        // }


        // concurrent computations
        startTime = System.currentTimeMillis();
        int[] rowSums = matrix.rowSumsConcurrent();
        usedTime = System.currentTimeMillis() - startTime;
        System.out.println("Concurrent execution took: " + usedTime + "ms");
        System.out.println("Result:");
        for (int i = 0; i < rowSums.length; i++) {
            System.out.println(i + " -> " + rowSums[i]);
        }
        
        // Just measure time of execution of your solution on provided matrix.
    }

}