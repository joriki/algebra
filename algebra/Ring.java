package algebra;

public interface Ring<T> {
	InvertibleBinaryOperation<T> addition ();
	BinaryOperationWithIdentity<T> multiplication ();
}
