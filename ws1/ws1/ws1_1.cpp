#include<iostream>
#include<fstream>
#include<string>
#include<cstdlib>
using namespace std;
class Data
{
	public:
	string Empid,Name,City,DOB,Gender,Doj,Department;
	string salary;
	int display();
};
int Data::display()
{
	cout<<Empid<<"\t"<<Name<<"\t"<<City<<"\t"<<DOB<<"\t"<<Gender<<"\t"<<Doj<<"\t"<<salary<<"\t"<<Department<<endl;
	return 0;
}
Data d[15];
int main()
{
	cout<<"Hello"<<endl<<"1.";
	ifstream fin;
	fin.open("data.txt");
	string s1;
	int i=0;
	while(!(fin.eof()))
	{
		getline(fin,s1,',');d[i].Empid=s1;
		getline(fin,s1,',');d[i].Name=s1;
		getline(fin,s1,',');d[i].City=s1;
		getline(fin,s1,',');d[i].DOB=s1;
		getline(fin,s1,',');d[i].Gender=s1;
		getline(fin,s1,',');d[i].Doj=s1;
		getline(fin,s1,',');d[i].salary=s1;//int a=stoi(s1);d[i].salary=a;
		getline(fin,s1);d[i].Department;
		i+=1;
	}
	for(int i=0;i<14;i++)
	{
		if(d[i].City=="Delhi")
			cout<<d[i].Name<<"\t";
	}	
	cout<<endl;
	int sum=0,max=0,min=9999999,i1=11,i2=11;
	for(int i=1;i<14;i++)
	{
		int a=stoi(d[i].salary);
		i1=(max>a)?i1:i;
                i2=(min<a)?i2:i;
		max=(max>a)?max:a;
		min=(min<a)?min:a;
		sum+=a;
	//	cout<<"The age of "<<i<<"is "<<10*(int(d[i].DOB[7])-int('0'))+int(d[i].DOB[8])-int('0');
	}
	cout<<"2.The total is "<<sum<<"\n3.The hifhest salary is "<<max<<"\n4.Name for highest is "<<d[i1].Name;
	cout<<"\n5.Person low salary is "<<d[i2].Name<<endl<<"6.";
	int a1=0,a2=99,i3=1,i4=11;
	for(int i=1;i<14;i++)
	{
		int a=10*(int(d[i].DOB[7])-int('0'))+(int(d[i].DOB[8])-int('0'));
		if(a==-48)
			a=70;
		int age=2019-(1900+a);
		cout<<"\nThe age of "<<i<<"is "<<2019-(1900+a);
		i3=(a1>age)?i3:i;
		i4=(a2<age)?i4:i;
		a1=(a1>age)?a1:age;
		a2=(a2<age)?a2:age;
	}
	cout<<"\n7.The senior is "<<d[i3].Name<<"\n8.The junior most is "<<d[i4].Name<<endl;	
	return 0;
}
