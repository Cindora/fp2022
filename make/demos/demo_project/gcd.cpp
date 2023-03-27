int gcd (int a, int b)
{
	while (b) {
		a %= b;
		int c = a;
		a = b;
		b = c;
	}
	return a;
}
