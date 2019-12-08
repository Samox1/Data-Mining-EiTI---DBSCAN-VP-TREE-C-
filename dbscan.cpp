#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

int main()
{
    cout << "Hello" << endl << "DBSCAN Test program" << endl;
    string file1 = "D:\\Programming\\Data-Mining-EiTI---DBSCAN-VP-TREE-C-\\DataCpp.csv";

    string line, word, temp;

    fstream plik;
    plik.open(file1);

    if(plik.good){
        while(!plik.eof())
        {
            getline(plik, line);
                
            while (getline(plik, word, ','))
            {
                cout << word;
            }

        cout << '\n'; 
        }
    }
    
    plik.close();
}