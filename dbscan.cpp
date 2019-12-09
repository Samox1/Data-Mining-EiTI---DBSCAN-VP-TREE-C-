#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

struct punkt{
    double x = 0.0;
    double y = 0.0;
    int cluster = -2;
};

int main()
{
    cout << "Hello" << endl << "DBSCAN Test program" << endl;
    string file1 = "DataCpp.csv";

    string line, word, temp;
    int index = 0;

    fstream plik;
    plik.open(file1);

    if(plik.good())
    {
        cout << "--- Opening file" << endl;
        while(!plik.eof())
        {
            while(getline(plik, line, '\n'))
            {
                // cout << index;
                // index++;
                // cout << line << endl;

                stringstream sstream(line);
                index = 0;
                while (getline(sstream, word, ','))
                {
                    //cout << word << " ";
                    if(index==1){
                        cout << "X: " << word << " ";
                    }else if(index==2){
                        cout << "Y: " << word << " ";
                    }
                    index++;
                }

                cout << endl;
            }

        //cout << '\n'; 
        }
    }else{
        cout << "Can't open file" << endl;
    }
    
    plik.close();
    
}