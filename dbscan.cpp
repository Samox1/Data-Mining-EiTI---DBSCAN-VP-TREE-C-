#include <iostream>
#include <math.h>
#include <fstream>
#include <sstream>
#include <chrono> 
#include <omp.h>


#define UNDEFINED   -2
#define NOISE       -1


using namespace std;
using namespace std::chrono; 


class Punkt{
    public:
        double *x;
        int cluster = UNDEFINED;
        void Przydziel(int);
        ~Punkt();
};
void Punkt::Przydziel(int N){
    x = new double[N];
}
Punkt::~Punkt(){
    delete [] x;
}


struct VP{
    public:
        int index_parent_node;      // Index wezla nadrzednego
        int index;                  // Index w tablicy wejsciowej
        char l_r;                   // Znak czy Lewo / Prawo (Left / Right)
        double mu;                  // Mediana odleglosci od punktu do pozostalych
};


void Import_CSV_Metadata(Punkt *pkt, int &ile_linii, int &ile_x, string &file_in, int &start_ind, int &start_linia);
void Import_CSV_File(Punkt *pkt, int ile_linii, int ile_x, string file_in, int start_ind, int start_linia);
void Show_Imported_First3(Punkt *pkt, int ile_linii, int ile_x);
void Show_Imported_All(Punkt *pkt, int ile_linii, int ile_x);
void Show_Clustered(Punkt *pkt, int ile_linii, int ile_x);
void Save_File(Punkt *pkt, int ile_linii, int ile_x);

void Clear_Cluster(Punkt *pkt, int ile_linii);

double DistFunc(Punkt *pkt1, Punkt *pkt2, int ile_x);
int RangeQuery(Punkt *pkt, int *N_tab, int ile_linii, int Qindex, double Eps, int ile_x);
int S_N_Merge(int *S_tab, int *N_tab, int S_licznik, int N_licznik, int ile_linii);
void DBSCAN_Origin(Punkt *pkt, int *S_tab, int *N_tab, double Eps, int ile_linii, int minN, int C, int ile_x);
//void DBSCAN_Origin_OMP(Punkt *pkt, int *S_tab, int *N_tab, double Eps, int ile_linii, int minN, int C, int ile_x);

double RandVal(double low,double high);
long iRandVal(long low,long high);
double Mediana(Punkt *pkt, double Mediana_tab[], int P_tab[], int D_tab[], int D_rozmiar, int ile_x, int p_counter);
int Select_VP(Punkt *pkt, int *S_VP_tab, int S_rozmiar, double P_proc_S, double D_proc_S, int ile_x);
void Make_VP_Tree(VP *VP_tree, Punkt *pkt, int *S_VP_tab, int S_rozmiar, double P_proc_S, double D_proc_S, int ile_x, int &Tree_Counter);



// --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN --- MAIN ---//

int main()
{
    cout << endl << "Hello, This is (VP-TREE) DBSCAN program" << endl;

    Punkt *pkt;                 // Wskaznik na "Punkt" --> potem przypisana tablica pod wskaznik
    int *N_tab;                 // Tablica z indeksami sasiadow przy RangeQuery
    int *S_tab;                 // Tablica "Seed" --> rozszerzajacy sie obszar sasiadow wokol punktu P

    int ile_linii = 0;          // Number of Rows    - Number of points 
    int ile_x = 0;              // Number of Columns - Number of co-ordinates for every point

    string file_in;             // String for File to Import - Variable to carry information between functions
    int start_ind = 0;          // Flag for import - take first COLUMN - Variable to carry information between functions
    int start_linia = 0;        // Flag for import - take first ROW - Variable to carry information between functions


// Import Metadata from CSV File    
    Import_CSV_Metadata(pkt, ile_linii, ile_x, file_in, start_ind, start_linia);

// Allocation Memory
    pkt = new Punkt[ile_linii];                                         // Memory Initialization

    for(int i=0; i<ile_linii; i++)
    {
        pkt[i].Przydziel(ile_x);                                        // Memory Initialization
    }

// Import Data from CSV File
    Import_CSV_File(pkt, ile_linii, ile_x, file_in, start_ind, start_linia);


// Show first 3 rows from memory - Imported Structure
    Show_Imported_First3(pkt, ile_linii, ile_x);

// Show ALL Imported Data
    // Show_Imported_All(pkt, ile_linii, ile_x);


// Test - Function - DBSCAN()
    int C = 0;                                                                  // Cluster Counter
    double Eps = 0.5;                                                           // Max distance between points
    int minN = 4;                                                               // Minimal number of Neighbors

    cout << endl << "Epsilon (double): ";
    cin >> Eps;
    cout << "Minimal number of Neighbors = minN (int)): ";
    cin >> minN;

    N_tab = new int[ile_linii];                                                 // Neighbors tab - index for N in pkt tab
    S_tab = new int[ile_linii];                                                 // Seed tab - index for S in pkt tab

    auto start = high_resolution_clock::now();                                  // Time - START
// DBSCAN --- START //
    DBSCAN_Origin(pkt, S_tab, N_tab, Eps, ile_linii, minN, C, ile_x);           // DBSCAN - Origin - Function
// DBSCAN --- END //
    auto stop = high_resolution_clock::now();                                   // Time - STOP
    auto duration = duration_cast<microseconds>(stop - start);                  // Time - Caltulation
    cout << endl << "DBSCAN Time: " << duration.count() << " us" << endl;       // Time - show Function duration

// Show every point and his Cluster number
    // Show_Clustered(pkt, ile_linii, ile_x);


// --- VP-TREE --------------------------------------------------------------------------------------------------------------- //
// Tworzenie drzewa Vantage Points

    int *S_VP_tab;
    S_VP_tab = new int[ile_linii];

    for (int i = 0; i < ile_linii; i++)                                         // Wpisywanie indeksow do startowej tablicy S do tworzenia drzewa VP-Tree
    {
        S_VP_tab[i] = i;
    }

    double P_proc_S = 0;
    double D_proc_S = 0;

    cout << "P Random Sample of S (0-1): " ;
    cin >> P_proc_S;
    cout << "D Random Sample of S (0-1): " ;
    cin >> D_proc_S;

 // Test - Function - Select_VP()   
    // int Best_p = Select_VP(pkt, S_VP_tab, ile_linii, P_proc_S, D_proc_S, ile_x);
    // cout << "Best_p = " << Best_p << endl;

    VP *VP_tree;                                                                // Memory for VP_Tree
    VP_tree = new VP[ile_linii];

    int Tree_Counter = -1;

    Make_VP_Tree(VP_tree, pkt, S_VP_tab, ile_linii, P_proc_S, D_proc_S, ile_x, Tree_Counter);

// --- VP-TREE --------------------------------------------------------------------------------------------------------------- //


// Saving CSV - with Cluster data
    Save_File(pkt, ile_linii, ile_x);


// delete - Destroy array/pointers
    delete [] S_tab;
    delete [] N_tab;
    delete [] VP_tree;
    delete [] pkt;

}
// --- End of Main --- //



void Make_VP_Tree(VP *VP_tree, Punkt *pkt, int *S_VP_tab, int S_rozmiar, double P_proc_S, double D_proc_S, int ile_x, int &Tree_Counter)
{
    if (S_rozmiar == 0)
    {
        return;
    }
    
    Tree_Counter++;

    VP_tree[Tree_Counter].index = Select_VP(pkt, S_VP_tab, S_rozmiar, P_proc_S, D_proc_S, ile_x);

    int p_counter = 0;
    int P_tabk = VP_tree[Tree_Counter].index;
    
    double *Mediana_tab;
    Mediana_tab = new double[S_rozmiar];

    VP_tree[Tree_Counter].mu = Mediana(pkt, Mediana_tab, &P_tabk, S_VP_tab, S_rozmiar, ile_x, p_counter);

    delete [] Mediana_tab;

    cout << "VP Index: " << VP_tree[Tree_Counter].index << " | Mediana: " << VP_tree[Tree_Counter].mu << endl;

}



double Mediana(Punkt *pkt, double Mediana_tab[], int P_tab[], int D_tab[], int D_rozmiar, int ile_x, int p_counter)
{
    double med = 0.0;

    // cout << "P_tab[p_counter]" << P_tab[p_counter] << endl;

    for (int d = 0; d < D_rozmiar; d++)
    {
        Mediana_tab[d] = DistFunc( &pkt[P_tab[p_counter]], &pkt[D_tab[d]], ile_x);      // Liczenie odleglosci od punktu P do wszystkich ze zbioru D
    }
        

    double temp_dist = 0;                                   // Sortowanie Babelkowe - START
    for (int b = 0; b < D_rozmiar; b++)                     
    {
        for (int c = 0; c < (D_rozmiar-1); c++)
        {
            if(Mediana_tab[c] > Mediana_tab[c+1]){
                temp_dist = Mediana_tab[c+1];
                Mediana_tab[c+1] = Mediana_tab[c];
                Mediana_tab[c] = temp_dist;
            }
        }
    }                                                       // Sortowanie Babelkowe - KONIEC


    if (D_rozmiar%2 == 1)                                   // D_rozmiar = Nieparzysta  - Mediana to srodkowa wartosc
    {
        med = Mediana_tab[D_rozmiar/2];
    }else                                                   // D_rozmiar = Parzysta     - Mediana to srednia z dwoch srodkowych wartosci
    {
        med = (Mediana_tab[(D_rozmiar/2) - 1] + Mediana_tab[(D_rozmiar/2)]) / 2.0;
    }

    return med;
}



int Select_VP(Punkt *pkt, int *S_VP_tab, int S_rozmiar, double P_proc_S, double D_proc_S, int ile_x){
    
    int P_rozmiar = P_proc_S * S_rozmiar;
    int D_rozmiar = D_proc_S * S_rozmiar;

    if(P_rozmiar < 1){
       P_rozmiar = 1; 
    }

    if(D_rozmiar < 1){
       D_rozmiar = 1; 
    }

    int *P_tab;
    int *D_tab;
    double *Mediana_tab;
    Mediana_tab = new double[D_rozmiar];
    P_tab = new int[P_rozmiar];
    D_tab = new int[D_rozmiar];

    int best_p = 0;
    double mu_pd = 0;
    double var = 0;
    
    int Si = 0;                                                 // Iterator po S
    int Pi = 0;                                                 // Iterator po P

    double best_spread = 0;

    while (Pi < P_rozmiar)                                      // Wylosowanie punktow P ze zbioru S
    {
        if ( RandVal(0,1) <= P_proc_S)
        {
            P_tab[Pi] = S_VP_tab[Si];
            
            Pi++;
        }
            
        Si++;
        
        if(Si >= S_rozmiar){                                    // Pirackie rozwiazanie problemu jesli wyjdziemy poza tablice S_VP_tab (poczatkowa) - zbieramy znowu punkty (mozliwe powtorzenia)
            Si = 0;
        }
    }
    
    // cout << "P_tab:" << endl;                                   // Wyswietlanie tablicy P
    // for (int i = 0; i < P_rozmiar; i++)
    // {
    //     cout << P_tab[i] << endl;
    // }
    
    
    for (int k = 0; k < P_rozmiar; k++)                         // Dla kazdego punktu p ze zbioru P
    {
        int Di = 0;                                             // Iterator po D

        Si = 0;
        while (Di < D_rozmiar)                                  // Wylosowanie punktow D ze zbioru S
        {
            if ( RandVal(0,1) <= D_proc_S)
            {
                D_tab[Di] = S_VP_tab[Si];
                Di++;
            } 
            Si++;
        
            if(Si >= S_rozmiar){                                // Pirackie rozwiazanie problemu jesli wyjdziemy poza tablice S_VP_tab (poczatkowa) - zbieramy znowu punkty (mozliwe powtorzenia)
                Si = 0;
            }
        }

        // cout << "D_tab:" << endl;                               // Wyswietlanie tablicy D
        // for (int i = 0; i < D_rozmiar; i++)
        // {
        //     cout << D_tab[i] << endl;
        // }


        double mu_pd = Mediana(pkt, Mediana_tab, P_tab, D_tab, D_rozmiar, ile_x, k);


        var = 0;                                                // Liczenie Wariancji - START
        for( int n = 0; n < D_rozmiar; n++)
        {
            var += pow(Mediana_tab[n] - mu_pd, 2);
        }
        var /= D_rozmiar;                                       // Liczenie Wariancji - KONIEC
        

        if (var > best_spread)                                  // Sprawdzenie punktu P - czy jest lepszy?        
        {
            best_spread = var;
            best_p = P_tab[k];
            // cout << "Better: " << best_p << " | War: " << best_spread << endl;
        }
    }


    delete [] D_tab;
    delete [] P_tab;

    return best_p;
}



// Additional functions

/************************ RandVal *****************************************/
double RandVal(double low,double high)
{
  return(((double) (rand() % RAND_MAX)/((double) RAND_MAX))*(high-low)+low);
}


/************************ iRandVal *****************************************/
long iRandVal(long low,long high) 
{
  high++;
  return (long) (((double) (rand() % RAND_MAX)/((double) RAND_MAX))*(high-low)+low);
}


// --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- FUNCTION --- //


void Import_CSV_Metadata(Punkt *pkt, int &ile_linii, int &ile_x, string &file_in, int &start_ind, int &start_linia)
{
    string file1 = "DataCpp.csv";                                           // Name of File to Import
    string line, word, temp, struktura;                                     // Temporary strings for import
    char FirstColYN, FirstRowYN;                                            // FirstColYN - char for: Import First Column?   // FirstRowYN - char for: Import First Row?
    int index = 0;                                                          // Counter for every "word" in imported line
    int counter_tab = 0;                                                    // Counter for "x" in every point
    int start_index = 0;                                                    // Start index - if User don't want First Column
    int ktora_linia = 0;                                                    // Row Counter for importing 
    int exist = 0;

    do{
        cout << "What file you want? : ";
        cin >> file1;
        if (FILE *file = fopen(file1.c_str(), "r")) {
            fclose(file);
            exist = 1;
        }else{
            cout << "Warning: Can't find this file" << endl;
        }
    }while(exist != 1);
    
    
    cout << "--- Open file: " << file1 << " --> ";

    fstream plik;                                                           // FStream for import file
    plik.open(file1);                                                       // Open import file

    if(plik.good())
    {
        //cout << plik.tellg() << endl;
        while(!plik.eof())
        {
            while(getline(plik, line, '\n'))                                // Calculate how many lines are in the file
            {
                ile_linii++;
            }
        }

        plik.clear();
        plik.seekg(0, ios::beg);                                            // Clear buffer and go to the beginning of the file

        for(int i = 0; i < 3; i++)                                          // Import first 3 lines from file - to show them later
        {   
            ile_x = 0;
            getline(plik, line, '\n');
            stringstream sstream_temp(line);
            while(getline(sstream_temp, word, ','))
            {
                // cout << word << " ";
                struktura = struktura + word + " | ";
                ile_x++;
            }
            struktura = struktura + '\n';
        }
        
        // cout << "Ile kolumn: " << ile_x << endl;
        // cout << "Wierszy w pliku: " <<  ile_linii << endl;
        cout << endl << "***" << endl << "File Structure: " << endl << "Columns: " <<  ile_x << endl << "Rows: " << ile_linii << endl << "Structure: " << endl << struktura << "***" << endl;       // Show - File Structure
        
        int flaga_wybor = 1;                                                // Flag reset
        while(flaga_wybor)                                                  // While - for: First Row Import - y / n 
        {   
            cout << "Import 1st row = Header? [y/n] : ";
            cin >> FirstRowYN;

            switch (FirstRowYN)
            {
            case 'y':
                ktora_linia = 0;
                flaga_wybor = 0;
                break;
            case 'n':
                ktora_linia = 1;
                ile_linii--;
                flaga_wybor = 0;
                break;
            default:
                cout << "Wrong character" << endl;
                break;
            } 
        }

        flaga_wybor = 1;                                                    // Flag reset
        while(flaga_wybor)                                                  // While - for: First Column Import - y / n 
        {   
            cout << "Import 1st column? [y/n] : ";
            cin >> FirstColYN;

            switch (FirstColYN)
            {
            case 'y':
                start_index = 0;
                flaga_wybor = 0;
                break;
            case 'n':
                start_index = 1;
                ile_x--;
                flaga_wybor = 0;
                break;
            default:
                cout << "Wrong character" << endl;
                break;
            } 
        }
    }else{
        cout << "Can't open file" << endl;
    }


    file_in = file1;
    start_linia = ktora_linia;
    start_ind = start_index;

}        



// Import File
void Import_CSV_File(Punkt *pkt, int ile_linii, int ile_x, string file_in, int start_ind, int start_linia)
{
    // string file1 = file_in;                                                 // Name of File to Import
    string line, word;                                                      // Temporary strings for import
    int index = 0;                                                          // Counter for every "word" in imported line
    int counter_tab = 0;                                                    // Counter for "x" in every point
    int start_index = start_ind;                                            // Start index - if User don't want First Column
    int ktora_linia = start_linia;                                          // Row Counter for importing 

    fstream plik;                                                           // FStream for import file
    plik.open(file_in);                                                       // Open import file

    if(plik.good())
    {
        plik.clear();
        plik.seekg(0, ios::beg);                                            // Clear buffer and go to the beginning of the file

        if(ktora_linia == 1){                                               // Get Line if FirstRowYN is NO - dont import first line
            getline(plik, line, '\n');
            ktora_linia = 0;                                                // Counter reset
        }

        while(!plik.eof())
        {
            while(getline(plik, line, '\n'))                                // Get every line from file
            {                
                stringstream sstream(line);
                index = 0;
                counter_tab = 0;
                // cout<<ktora_linia<<": "<<endl;                           // Show - which point is being imported
                while (getline(sstream, word, ','))                         // Get every "word" (number) from line
                {
                    if(index>=start_index)
                    {
                        pkt[ktora_linia].x[counter_tab] = atof(word.c_str());                                           // Conversion string to double
                        // cout<<index<<" "<<counter_tab<<" "<<word <<" = " <<pkt[ktora_linia].x[counter_tab] <<endl;   // Show - data from memory while importing
                        counter_tab++;

                    }
                    index++; 
                }
                ktora_linia++;
            }
        }

        cout << "Close file: Imported "<<ile_x<<"D data" << endl;

    }else{
        cout << "Can't open file" << endl;
    }

    plik.close();
    
}


void Show_Imported_All(Punkt *pkt, int ile_linii, int ile_x)
{
    cout << endl << "***" << endl << "ALL - Imported Structure in Memory: " << endl << "Columns: " <<  ile_x << endl << "Rows: " << ile_linii << endl << "Structure in Memory: " << endl;
    for(int i=0; i<ile_linii; i++)
    {
        cout<<i<<": ";
        for (int j = 0; j < ile_x; j++)
        {
            cout << "x[" << j << "] = " << pkt[i].x[j] << " | ";
        }
        cout << endl;
    }
}


void Show_Imported_First3(Punkt *pkt, int ile_linii, int ile_x) 
{
    cout << endl << "***" << endl << "First3 - Imported Structure in Memory: " << endl << "Columns: " <<  ile_x << endl << "Rows: " << ile_linii << endl << "Structure in Memory: " << endl;       // Show - Imported Structure in Memory
    for(int i=0; i<3; i++)
    {
        cout << i << ": ";
        for (int j = 0; j < ile_x; j++)
        {
            cout << "x[" << j << "] = " << pkt[i].x[j] << " | ";
        }
        cout << endl;
    }
    cout << "***" << endl;
}


void Show_Clustered(Punkt *pkt, int ile_linii, int ile_x)
{
    cout << endl;
    for(int i=0; i<ile_linii; i++)
    {
        cout << i << ": ";
        for (int j = 0; j < ile_x; j++)
        {
            cout << pkt[i].x[j] << ",";
        }
        cout << pkt[i].cluster << endl;
    }
}



void Save_File(Punkt *pkt, int ile_linii, int ile_x)
{
    string file_out = "Data_Clustered.csv";
    cout << endl << "File output: ";
    cin >> file_out;
    int num_file_out = 0;
    int exist = 1;

    do
    {
        if (file_out.find(".csv") == -1){
            file_out = file_out + ".csv";
        }

        if (FILE *file = fopen(file_out.c_str(), "r")) 
        {
            fclose(file);
            cout << "Oh there is file: \"" << file_out << "\" - You wanna override this? [y/n]: ";
            char OverYN;
            cin >> OverYN;
            if (OverYN == 'y')
            {
                exist = 0;
            }
            else if(OverYN == 'n'){
                cout << "There is file: " << file_out << " --> Changing name to: ";
                file_out.erase(file_out.length()-4, file_out.length());
                num_file_out++;
                if(num_file_out > 1)
                {
                    file_out.pop_back();
                }
                file_out = file_out + to_string(num_file_out) + ".csv";
                cout << file_out << endl;
                exist = 1;
            }
            
        }else{
            exist = 0;
        }
    } while(exist == 1);
    

    ofstream plik_out;

    cout << "--- Open CSV: " << file_out << " --> ";

    plik_out.open(file_out);

    if(plik_out.good())
    {   
        for (int i = 0; i < ile_linii; i++)
        {
            //plik_out << pkt[i].x[0] << "," << pkt[i].x[1] << "," << pkt[i].cluster << endl;
            for (int j = 0; j < ile_x; j++)
            {
                plik_out << pkt[i].x[j] << ",";
            }
            plik_out << pkt[i].cluster << endl;
        }
        
    }else{
        cout << "Can't open file" << endl;
    }
    plik_out.close();
    cout << "Out File: Closed" << endl << endl;
}



void Clear_Cluster(Punkt *pkt, int ile_linii)
{
    for(int i=0; i<ile_linii; i++)
    {
        pkt[i].cluster = -2;
    }
}



double DistFunc(Punkt *pkt1, Punkt *pkt2, int ile_x)                // Function to calculate distance 
{ 
    double distance = 0.0;
    // Calculating distance 
    for(int i=0; i<ile_x; i++){
        distance = distance + pow(pkt2->x[i] - pkt1->x[i], 2);
    }
    distance = sqrt(distance);
    
    return distance;
} 

int RangeQuery(Punkt *pkt, int *N_tab, int ile_linii, int Qindex, double Eps, int ile_x)
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
            if(DistFunc(&pkt[Qindex], &pkt[i], ile_x) <= Eps)
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



void DBSCAN_Origin(Punkt *pkt, int *S_tab, int *N_tab, double Eps, int ile_linii, int minN, int C, int ile_x)
{

for(int P = 0; P < ile_linii; P++)
{
    if(pkt[P].cluster != UNDEFINED)
    {
        continue;
    }

    int ile_sasiadow = RangeQuery(pkt, N_tab, ile_linii, P, Eps, ile_x);
    
    // cout << "Punkt: " << P << " -> x: " << pkt[P].x << " | y: " << pkt[P].y << endl;
    // cout << "Ile sasiadow: " << ile_sasiadow << endl << endl;

    if(ile_sasiadow < minN)
    {
        pkt[P].cluster = NOISE;             // IF: N < minN => NOISE
        continue;
    }

    C = C + 1;                              // Cluster number increment
    pkt[P].cluster = C;                     // ELSE: N > minN => Cluster
    
    // cout << "Punkt: " << P << " | Cluster: " << pkt[P].cluster << endl;     // Show changing Cluster number
    
    
    for(int i=0; i<ile_linii; i++)
    {
        S_tab[i] = N_tab[i];           
    }

    int S_licznik = ile_sasiadow;
  
    for(int i=0; i<S_licznik; i++)                      // For Each Point Q in S
    {
        if(S_tab[i] != -1)
        {
            if(pkt[S_tab[i]].cluster == NOISE)          // IF: Q == NOISE then Q = Cluster
            {
                pkt[S_tab[i]].cluster = C;
            }

            if(pkt[S_tab[i]].cluster != UNDEFINED)      // IF: Q == NOISE or CLUSTER then leave Q
            {     
                continue;
            }

            pkt[S_tab[i]].cluster = C;
            ile_sasiadow = RangeQuery(pkt, N_tab, ile_linii, S_tab[i], Eps, ile_x);

            if( ile_sasiadow >= minN)
            {
                S_licznik = S_N_Merge(S_tab, N_tab, S_licznik, ile_sasiadow, ile_linii);
            }
        }
    }
}
}






// NOT WORKING PROPERLY ------------------------------------------------------------------------------------------ //

/*

void DBSCAN_Origin_OMP(Punkt *pkt, int *S_tab, int *N_tab, double Eps, int ile_linii, int minN, int C, int ile_x)
{

//#pragma omp parallel for
for(int P = 0; P < ile_linii; P++)
{
    if(pkt[P].cluster != UNDEFINED)
    {
        continue;
    }

    int ile_sasiadow = RangeQuery(pkt, N_tab, ile_linii, P, Eps, ile_x);
    
    // cout << "Punkt: " << P << " -> x: " << pkt[P].x << " | y: " << pkt[P].y << endl;
    // cout << "Ile sasiadow: " << ile_sasiadow << endl << endl;

    if(ile_sasiadow < minN)
    {
        pkt[P].cluster = NOISE;             // IF: N < minN => NOISE
        continue;
    }

    C = C + 1;                              // Cluster number increment
    pkt[P].cluster = C;                     // ELSE: N > minN => Cluster
    
    // cout << "Punkt: " << P << " | Cluster: " << pkt[P].cluster << endl;     // Show changing Cluster number
    
    
    for(int i=0; i<ile_linii; i++)
    {
        S_tab[i] = N_tab[i];           
    }

    int S_licznik = ile_sasiadow;
  
    for(int i=0; i<S_licznik; i++)                      // For Each Point Q in S
    {
        if(S_tab[i] != -1)
        {
            if(pkt[S_tab[i]].cluster == NOISE)          // IF: Q == NOISE then Q = Cluster
            {
                pkt[S_tab[i]].cluster = C;
            }

            if(pkt[S_tab[i]].cluster != UNDEFINED)      // IF: Q == NOISE or CLUSTER then leave Q
            {     
                continue;
            }

            pkt[S_tab[i]].cluster = C;
            ile_sasiadow = RangeQuery(pkt, N_tab, ile_linii, S_tab[i], Eps, ile_x);

            if( ile_sasiadow >= minN)
            {
                S_licznik = S_N_Merge(S_tab, N_tab, S_licznik, ile_sasiadow, ile_linii);
            }
        }
    }
}
   // END OMP parallel
}

*/