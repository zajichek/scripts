
public final class Poisson {
	
	//Calculate on log scale then exponentiate
	public static double logPmf(int X, double lambda) {
		if(X == 0) {
			return -1*lambda;
		}
		
		double num = -1*lambda + X*Math.log(lambda);
		
		double denom = Math.log(X);
		int i = 1;
		while(X > i) {
			denom = denom + Math.log(X - i);
			i++;
		}
		
		return num - denom;
	}
}
