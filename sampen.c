#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int N;
int M;
double R;

/*  Compute the sum of all elements and the average */
double c_mean(double* vector, int option)
{
	int i;
	double sum = 0;
	double average = 0;
	double result = 0;
	for (i = 0; i < N; i++)
	{
		sum = sum + *vector;
		vector++;
	}

	average = sum / (double)N;
	if (option == 0) result = sum;
	else result = average;
	return result;
}

/*  Compute  variance  and standard deviation  */
double c_std(double* vector)
{
	int i;
	double sum1 = 0;
	double average = 0;
	double variance = 0;
	double std_deviation = 0;
	average = c_mean(vector, 1);
	for (i = 0; i < N; i++)
	{
		sum1 = sum1 + (double)pow((*vector - average), 2);
		vector++;
	}
	variance = sum1 / (double)N;
	std_deviation = sqrt(variance);

	return std_deviation;
}

/*  Normalize   */
void c_normalize(double* vector)
{
	int i;
	double average_v = 0;
	double std_v = 0;
	std_v = c_std(vector);
	average_v = c_mean(vector, 1);
	for (i = 0; i < N; i++)
	{
		*vector = (*vector - average_v) / std_v;
		vector++;
	}

}

double c_max(double* vector, int L)
{
	int i;
	double max = 0;
	double num = 0;
	for (i = 0; i < L; i++)
	{
		num = *vector;
		if (num > max) max = *vector;
		vector++;
	}
	return max;
}

double c_max_C(double a, double b)
{
	double max = a;
	if (b > a) max = b;
	return max;
}

double c_maxV(double vector[M], int L)
{
	int i;
	double max = vector[0];
	for (i = 1; i < L; i++)
	{
		if (vector[i] > max) max = vector[i];
	}
	return max;
}


void sampen(double* x, int* n, int* m, double* r, int* At, int* Bt, double* E)
{
	//cross_sampen(x,y,m,r,At,Bt,E,n)
	//
	//Inputs:
	//x: time-serie (array)
	//y: time-serie (array)
	//m: template length m (vector size), (Suggested m=2)
	//r: matching tolerance (Suggested r=0.2)
	//At: total matches for size vector m+1 (Initialize with 0)
	//Bt: total matches for size vector m (Initialize with 0)
	//E: Value of Cross-Entropy (Initialize with 0)
	//n: Size of x or y (The time series x and y must have the same size)
	//
	//Outputs :
	//
	//E : Cross sample entropy estimates for m 
	//At: total matches for size vector m+1 
	//Bt: total matches for size vector m 

	N = *n;
	M = *m;
	R = *r;
	int i, j, k;
	int lim;
	double X[N];
	//double Y[N];
	double A;
	double B;
	double p;
	double dist[M];
	double max_td;
	double AT = 0;
	double BT = 0;
	double* pdist;

	pdist = &dist[0];
	lim = N - M + 1;

	c_normalize(x);
	//c_normalize(y);

	for (i = 0; i < N; i++)
	{
		X[i] = *x;
		//Y[i] = *y;
		x++;
		//y++;
	}

	for (i = 0; i < lim-M; i++)
	{
		for (j = i+1; j < lim; j++)
		{
			if (i != j)
			{			
				for (k = 0; k< M; k++)
				{
					dist[k] = fabs(X[i + k] - X[j + k]);
				}
				max_td = c_max(dist, M);
				if (max_td < R) BT++;
				if (i < (N - M) && j < (N - M))
				{
					if (c_max_C(max_td, (fabs(X[i + M] - X[j + M]))) < R) AT++;
				}
			}

		}

	}

	*At = AT;
	*Bt = BT;
	B = AT / (double)pow((N - M), 2);
	A = BT / (double)pow((N - M), 2);
	p = A / B;
	*E = log(p);
}
