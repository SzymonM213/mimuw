package przyklady02;

import java.util.Random;

public class Vectors {

    public static class Vector {

        private static final int LENGTH = 10;

        public int values[];

        public Vector(int values[]) {
            this.values = new int[values.length];
            for(int i = 0; i < values.length; i++) {
                this.values[i] = values[i];
            }
        }

        private static class Sum implements Runnable {
            
            private final int[] values1;
            private final int[] values2;
            private final int[] result;
            private final int start;

            public Sum(int[] values1, int[] values2, int[] result, int start) {
                this.values1 = values1;
                this.values2 = values2;
                this.result = result;
                this.start = start;
            }

            @Override
            public void run() {
                for (int i = start; i < start + LENGTH && i < result.length; i++) {
                    this.result[i] = this.values1[i] + this.values2[i];
                }
            }
        }

        private static class Dot implements Runnable {
            
            private final int[] values1;
            private final int[] values2;
            private final int[] result;
            private final int start;

            public Dot(int[] values1, int[] values2, int[] result, int start) {
                this.values1 = values1;
                this.values2 = values2;
                this.result = result;
                this.start = start;
            }

            @Override
            public void run() {
                int tmp = 0;
                for (int i = start; i < start + LENGTH && i < this.values1.length; i++) {
                    tmp += this.values1[i] * this.values2[i];
                }
                result[start / LENGTH] = tmp;
            }
        }

        public Vector sum(Vector other) {
            Thread threads[] = new Thread[(int) Math.ceil(this.values.length / LENGTH) + 1];
            int result[] = new int[this.values.length];
            for (int i = 0; i < threads.length; i++) {
                threads[i] = new Thread(new Sum(this.values, other.values, result, i * LENGTH));
                threads[i].start();
            }
            for (Thread t: threads) {
                try {
                    t.join();
                } catch (InterruptedException e) {}
            }
            return new Vector(result);
        }

        public int dot(Vector other) {
            Thread threads[] = new Thread[(int) Math.ceil(this.values.length / LENGTH) + 1];
            int result[] = new int[threads.length];
            for (int i = 0; i < threads.length; i++) {
                threads[i] = new Thread(new Dot(this.values, other.values, result, i * LENGTH));
                threads[i].start();
            }
            for(Thread t: threads) {
                try {
                    t.join();
                } catch (InterruptedException e) {}
            }
            int sum = 0;
            for(int n: result) sum += n;
            return sum;
        }

        public Vector sumSeq(Vector other) {
            int result[] = new int[this.values.length];
            for (int i = 0; i < result.length; i++) {
                result[i] = this.values[i] + other.values[i];
            }
            return new Vector(result);
        }

        public int dotSeq(Vector other) {
            int result = 0;
            for (int i = 0; i < this.values.length; i++) {
                result += this.values[i] * other.values[i];
            }
            return result;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            for (int n: this.values) {
                sb.append(n).append(" ");
            }
            return sb.toString();
        }
    }

    public static void main(String[] args) {
        Random r = new Random();
        int maxValue = 999;
        int vectorLength = r.nextInt(maxValue);
        int values1[] = new int[vectorLength];
        int values2[] = new int[vectorLength];
        for(int i = 0; i < vectorLength; i++) {
            values1[i] = r.nextInt(maxValue);
            values2[i] = r.nextInt(maxValue);
        }
        Vector vector1 = new Vector(values1);
        Vector vector2 = new Vector(values2);
        Vector sumResult = vector1.sum(vector2);
        Vector sumSeqResult = vector1.sumSeq(vector2);
        int dotResult = vector1.dot(vector2);
        int dotSeqResult = vector1.dotSeq(vector2);

        boolean correctDot = dotResult == dotSeqResult;
        boolean correctSum = true;
        for (int i = 0; i < vectorLength; i++) {
            if (sumResult.values[i] != sumSeqResult.values[i]) correctSum = false; 
        }
        if (correctSum) System.out.println("sumSeq result equals sum result");
        else System.out.println("sumSeq result does not equal sum result!");
        if (correctDot) System.out.println("dotSeq result equals dot result");
        else System.out.println("dotSeq result does not equal dot result");
    }

}
