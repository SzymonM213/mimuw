package przyklady06;

import java.util.LinkedList;

public class BlockingQueue<T> {

    private LinkedList<T> elements;
    private int capacity;

    public BlockingQueue(int capacity) {
        this.elements = new LinkedList<>();
        this.capacity = capacity;
    }

    public synchronized T take() throws InterruptedException {
        while (this.elements.isEmpty()) {
            wait();
        }
        T tmp = this.elements.pop();
        notifyAll();
        return tmp;
    }

    public synchronized void put(T item) throws InterruptedException {
        while (this.elements.size() == this.capacity) {
            wait();
        }
        this.elements.push(item);
        notifyAll();
    }

    public synchronized int getSize() {
        return this.elements.size();
    }

    public int getCapacity() {
        return this.capacity;
    }
}