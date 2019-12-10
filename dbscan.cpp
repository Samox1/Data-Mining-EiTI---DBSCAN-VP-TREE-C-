#include <iostream>
#include <math.h>
#include <omp.h>
#include <fstream>
#include <sstream>

using namespace std;


struct Punkt{
    double x = 0.0;
    double y = 0.0;
    int cluster = -2;
};


double DistFunc(Punkt pkt1, Punkt pkt2);


int main()
{
    cout << "Hello" << endl << "DBSCAN Test program" << endl;
    
    Punkt *pkt;         // Wskaznik na "Punkt" --> potem przypisana tablica pod wskaznik
    
    string file1 = "DataCpp.csv";
    string line, word, temp;
    int index = 0;
    int ile_linii = 0;

    fstream plik;
    plik.open(file1);

    if(plik.good())
    {
        //cout << plik.tellg() << endl;
        while(!plik.eof())
        {
            while(getline(plik, line, '\n'))
            {
                ile_linii++;
            }
        }
        //cout << plik.tellg() << endl;
        plik.clear();
        plik.seekg(0, ios::beg);
        //cout << plik.tellg() << endl;
        //cout << "Ile linii: " <<  ile_linii << endl;
        pkt = new Punkt[ile_linii];

        int ktora_linia = 0;

        cout << "--- Opening file" << endl;
        while(!plik.eof())
        {
            while(getline(plik, line, '\n'))
            {                
                stringstream sstream(line);
                index = 0;
                while (getline(sstream, word, ','))
                {
                    if(index==1){
                        pkt[ktora_linia].x = atof(word.c_str());
                        //cout << "X: " << word << " ";
                    }else if(index==2){
                        pkt[ktora_linia].y = atof(word.c_str());
                        //cout << "Y: " << word << " ";
                    }
                    index++;
                }
                ktora_linia++;
                cout << endl;
            }
        }
    }else{
        cout << "Can't open file" << endl;
    }
    plik.close();

// Wyswietlanie wczytanych danych
/*
    for(int i=0; i<ile_linii; i++){
        cout << "x: " << pkt[i].x << " | y:" << pkt[i].y << endl;
    }
*/

// Test - Funkcji DistFunc

    cout << DistFunc(pkt[15], pkt[20]) << endl;
    


// Koniec programu

    delete [] pkt;
}



// --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- //

// Function to calculate distance 
double DistFunc(Punkt pkt1, Punkt pkt2) 
{ 
    // Calculating distance 
    return sqrt(pow(pkt2.x - pkt1.x, 2) +  pow(pkt2.y - pkt1.y, 2) * 1.0); 
} 