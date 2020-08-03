#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;

unsigned int myseed = 0x015A4E36;
const int N = 50000;
//const int N = 5;

int myrand(void)
{
  unsigned int t = myseed * 0x015A4E35 + 1;
  myseed = t;
  return (int)(t >> 16) & 0x7FFF;
}

void mysrand(unsigned int seed)
{
  myseed = seed;
  myrand();
}

int main(void)
{
  unsigned t = time(NULL);
  unsigned int i = 0;
  vector<int> rng(N, 1);
  
  std::ofstream outfile;

  //srand(t);
  //mysrand(t);
  
  std::ofstream myfile;
  myfile.open("../Data/PRNG/borland-50k.csv");
  for(i = 0; i < N; i++){
      rng.at(i) = myrand(); 
      cout << i+1 << ": " << rng.at(i) << '\n';
      myfile << rng.at(i) << '\n';  	
      
  }

  // Close the file
  myfile.close();
  return 0;
}
