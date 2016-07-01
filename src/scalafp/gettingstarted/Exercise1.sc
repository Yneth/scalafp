object Exercise1 {
	def fibonacci(n : Int) : Int = {
        def fibIter(a: Int, b: Int, n: Int) : Int = {
            if (n == 1) b
            else fibIter(b, a + b, n - 1)
        }
        if (n == 0) 0
        else fibIter(0, 1, n)
    }                                             //> fibonacci: (n: Int)Int
    for (i <- 0 to 10) print(fibonacci(i) + " ")
}