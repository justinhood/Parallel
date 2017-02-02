#include<stdio.h>
int main(){
	float total, hours, hourly;
	setbuf(stdout, NULL);
	printf ("Enter the hourly pay rate and the number of hours worked separated by a space.\n");
 	scanf ("%f %f", &hourly, &hours);
    	total = hours*hourly;
      	printf ("Your total pay is: $%.2f", total);
}
