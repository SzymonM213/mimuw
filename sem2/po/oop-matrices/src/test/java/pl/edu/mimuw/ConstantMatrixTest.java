package pl.edu.mimuw;

import org.junit.jupiter.api.Test;
import pl.edu.mimuw.matrix.IDoubleMatrix;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.*;
import static pl.edu.mimuw.matrix.Shape.matrix;

class ConstantMatrixTest {

    @Test
    void testPathologicalArgumentConstant() {
        assertThrows(AssertionError.class, () -> constant(null, 0));
    }

    @Test
    void testPlusConstant() {
        IDoubleMatrix m1 = constant(matrix(5, 7), 69);
        IDoubleMatrix m2 = constant(matrix(5, 7), 420);
        IDoubleMatrix m3 = constant(matrix(50, 17), 69);

        assertEquals(489, m1.plus(m2).get(0, 0));
        assertThrows(AssertionError.class, () -> m1.plus(m3));
    }

    @Test
    void testNormColumn() {
        IDoubleMatrix m1 = constant(matrix(3, 3), 69);

        assertEquals(207, m1.normOne());
        assertEquals(207, m1.normInfinity());
        assertEquals(207, m1.frobeniusNorm());
    }
}
