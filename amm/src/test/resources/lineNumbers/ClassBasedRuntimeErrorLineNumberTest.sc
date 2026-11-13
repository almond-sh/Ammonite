// Code to check runtime error line numbers with the class-based code wrapper

def boom(): Int = throw new Exception("boom")

@

val n = boom()
