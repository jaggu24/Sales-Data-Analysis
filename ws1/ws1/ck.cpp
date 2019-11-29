#include<iostream>
#include<string>
#include<cstdlib>
using namespace std;
int main()
{
	string g;
	g="123 ";
	int s=stoi(g);
	cout<<s+23<<endl<<*g[1];
	return 0;
}
