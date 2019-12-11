#include <iostream>
#include <math.h>
#include <omp.h>
#include <fstream>
#include <sstream>

#define UNDEFINED   -2
#define NOISE       -1


using namespace std;


struct Punkt{
    double x = 0.0;
    double y = 0.0;
    int cluster = UNDEFINED;
};


double DistFunc(Punkt *pkt1, Punkt *pkt2);
int RangeQuery(Punkt *pkt, int *N_tab, int ile_linii, int Qindex, double Eps);
int S_N_Merge(int *S_tab, int *N_tab, int S_licznik, int N_licznik, int ile_linii) ;


int main()
{
    cout << "Hello" << endl << "DBSCAN Test program" << endl;
    
    Punkt *pkt;         // Wskaznik na "Punkt" --> potem przypisana tablica pod wskaznik
    int *N_tab;         // Tablica z indeksami sasiadow przy RangeQuery
    int *S_tab;         // Tablica "Seed" --> 
    int *Temp_tab;

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
                //cout << endl;
            }
        }
    }else{
        cout << "Can't open file" << endl;
    }
    plik.close();
    cout << "--- Close file" << endl;

// Wyswietlanie wczytanych danych
/*
    for(int i=0; i<ile_linii; i++){
        cout << "x: " << pkt[i].x << " | y:" << pkt[i].y << endl;
    }
*/

// Test - Funkcji DistFunc

    int wsp1 = 15;
    int wsp2 = 20;
    cout << "Distance between pkt: "<< wsp1 << " & "<< wsp2 << " = " << DistFunc(&pkt[wsp1], &pkt[wsp2]) << endl;
    
    int C = 0;                      // Cluster Counter
    double Eps = 0.5;               // Max distance between points
    int minN = 4;                   // Minimal number of Neighbors

    int Qindex = 10;

    N_tab = new int[ile_linii];
    S_tab = new int[ile_linii];
    Temp_tab = new int[ile_linii];


    // for(int i=0; i<ile_linii; i++)
    // {
    //     if(pkt[i].cluster != UNDEFINED){
    //         continue;
    //     }

    // }


    int ile_sasiadow = RangeQuery(pkt, N_tab, ile_linii, Qindex, Eps);
    cout << "Ile sasiadow: " << ile_sasiadow << endl << endl;
    
    cout << "Punkt: " << Qindex << "x: " << pkt[Qindex].x << " | y: " << pkt[Qindex].y << endl;

    for(int i=0; i<ile_sasiadow; i++)
    {
        cout << N_tab[i] << " ~ x: " << pkt[N_tab[i]].x << " | y: " << pkt[N_tab[i]].y << endl;
    }

    if(ile_sasiadow < minN)
    {
        pkt[Qindex].cluster = NOISE;                // IF: N < minN => NOISE
        //continue;
    }else
    {       
        pkt[Qindex].cluster = C;                    // ELSE: N > minN => Cluster
    }
    
    cout << "Punkt: " << Qindex << " | Cluster: " << pkt[Qindex].cluster << endl;

    for(int i=0; i<ile_linii; i++)
    {
        S_tab[i] = N_tab[i];   
        //cout << S_tab[i] << endl;           
    }

    int S_licznik = ile_sasiadow;
    // int N_licznik = 66;


    // for(int i=0; i<N_licznik; i++)
    // {
    //     N_tab[i] = i;   
    //     //cout << S_tab[i] << endl;           
    // }


    // // Test - MERGE 2 ARRAYS w/o duplicates
    // int ile_Seed = S_N_Merge(S_tab, N_tab, N_licznik, N_licznik, ile_linii);

    // for(int i = 0; i <ile_linii; i++) 
    // { 
    //     cout << S_tab[i] << " ";
    // } 


    for(int i=0; i<S_licznik; i++)               // For Each Point Q in S
    {
        if(S_tab[i] != -1)
        {
            if(pkt[S_tab[i]].cluster == NOISE)          // IF: Q == NOISE then Q = Cluster
            {
                pkt[S_tab[i]].cluster = C;
            }

            if(pkt[S_tab[i]].cluster != UNDEFINED){     // IF: Q == NOISE or CLUSTER then leave Q
                continue;
            }

            //i = 0;

            pkt[S_tab[i]].cluster = C;
            ile_sasiadow = RangeQuery(pkt, N_tab, ile_linii, Qindex, Eps);

            if( ile_sasiadow >= minN)
            {
                S_licznik = S_N_Merge(S_tab, N_tab, S_licznik, ile_sasiadow, ile_linii);

                cout << "Array after merging" <<endl; 
                for (int i=0; i < ile_linii; i++) 
                {
                    cout << S_tab[i] << " ";
                }
                cout << "-------------------" << endl;
                    //}
                // }
            }
        }
    }

cout << endl;
    for(int i=0; i<ile_linii; i++)
    {
        cout << i << "C: " << pkt[i].cluster << endl;              // Wyswietlenie N_tab (calej)
    }







    // cout << "Cala N_tab: " << endl;
    // for(int i=0; i<ile_linii; i++)
    // {
    //     cout << N_tab[i] << endl;              // Wyswietlenie N_tab (calej)
    // }

// Koniec programu

    delete [] Temp_tab;
    delete [] S_tab;
    delete [] N_tab;
    delete [] pkt;
}



// --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- //

// Function to calculate distance 
double DistFunc(Punkt *pkt1, Punkt *pkt2) 
{ 
    // Calculating distance 
    return sqrt(pow(pkt2->x - pkt1->x, 2) +  pow(pkt2->y - pkt1->y, 2) * 1.0); 
} 

int RangeQuery(Punkt *pkt, int *N_tab, int ile_linii, int Qindex, double Eps)
{
    for(int i=0; i<ile_linii; i++)
    {
        N_tab[i] = -1;              // Czyszczenie listy indeksow sasiadow
    }

    int j = 0;

    for(int i=0; i<ile_linii; i++)
    {
        if(i != Qindex)
        {
            if(DistFunc(&pkt[Qindex], &pkt[i]) <= Eps)
            {
                N_tab[j] = i;
                j++;
            }
        }
    }

    return j;
}


int S_N_Merge(int *S_tab, int *N_tab, int S_licznik, int N_licznik, int ile_linii) 
{ 
    int flaga = 0; 
    
    for(int i=0; i<N_licznik; i++)
    {
        flaga = 0;

        for(int j=0; j<S_licznik; j++)
        {
            if(S_tab[j] == N_tab[i])
            {
                flaga = 1;
            }
        }

        if(flaga == 0)
        {
            S_tab[S_licznik] = N_tab[i];
            S_licznik++;
        }

        if (S_licznik == ile_linii)
        {
            break;
        }
    }

    return S_licznik;
}

    // while (k < ile_linii)
    // {
        // if(S_tab[i] < 0 && N_tab[j] < 0)
        // {
        //     Temp_tab[k++] = -1;
        // }
        // else
        // {
        //     if(S_tab[i] < N_tab[j])
        //     {
        //         if(S_tab[i] >= 0)
        //         {
        //             Temp_tab[k++] = S_tab[i++];
        //         }
                
        //     }else{
        //         if(N_tab[j] >= 0)
        //         {
        //             Temp_tab[k++] = N_tab[j++];
        //         }
        //     }
        // }

///JJJJ
// k = k-1;
// cout << S_tab[i] << "x" << N_tab[j] << "x"<< Temp_tab[k] <<endl;
// k = k+1;

//         if(S_tab[i] < 0)
//         {
//             if(N_tab[j] < 0)
//             {
//                 Temp_tab[k++] = -1;
//             }
//             else
//             {
//                 Temp_tab[k++] = N_tab[j++];
//             }
//         }
//         else
//         {
//             if(N_tab[j] < 0)
//             {
//                 Temp_tab[k++] = S_tab[i++];
//             }
//             else
//             {
//                 if(Temp_tab[k] == S_tab[i])
//                 {
//                     i++;
//                 }else if(Temp_tab[k] == N_tab[j]){
//                     j++;
//                 }
//                 else
//                 {
//                     if(S_tab[i] < N_tab[j])
//                     {
//                         Temp_tab[k++] = S_tab[i++];
//                     }else if(S_tab[i] == N_tab[j])
//                     {
//                         i++;
//                     }
//                     else
//                     {
//                         Temp_tab[k++] = N_tab[j++];
//                     }
//                 }
                
//             }
//         }
//         cout << i << "|" << j << "|"<< k <<endl;
//         //cout << S_tab[i--] << "|" << N_tab[j--] << "|"<< Temp_tab[k] <<endl;
    // }
    

//} 