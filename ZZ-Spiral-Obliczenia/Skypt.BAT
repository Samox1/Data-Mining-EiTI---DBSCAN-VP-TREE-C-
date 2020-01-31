FOR /L %%A IN (1,1,3) DO FOR /L %%E IN (2,1,4) DO FOR /L %%N IN (2,1,4) DO .\DBScan_VPTree.exe Spiral.csv n n %%A %%E %%N 0.01 0.01 Spiral_Out2_%%A_%%E_%%N.txt
FOR /L %%A IN (2,1,2) DO FOR /L %%E IN (3,1,3) DO FOR /L %%N IN (3,1,3) DO .\DBScan_VPTree.exe Spiral.csv n n %%A %%E %%N 0.1 0.1 Spiral_Out3_%%A_%%E_%%N.txt
FOR /L %%A IN (2,1,2) DO FOR /L %%E IN (3,1,3) DO FOR /L %%N IN (3,1,3) DO .\DBScan_VPTree.exe Spiral.csv n n %%A %%E %%N 0.2 0.2 Spiral_Out3_%%A_%%E_%%N.txt
FOR /L %%A IN (2,1,2) DO FOR /L %%E IN (3,1,3) DO FOR /L %%N IN (3,1,3) DO .\DBScan_VPTree.exe Spiral.csv n n %%A %%E %%N 0.3 0.3 Spiral_Out3_%%A_%%E_%%N.txt
FOR /L %%A IN (2,1,2) DO FOR /L %%E IN (3,1,3) DO FOR /L %%N IN (3,1,3) DO .\DBScan_VPTree.exe Spiral.csv n n %%A %%E %%N 0.4 0.4 Spiral_Out3_%%A_%%E_%%N.txt
FOR /L %%A IN (2,1,2) DO FOR /L %%E IN (3,1,3) DO FOR /L %%N IN (3,1,3) DO .\DBScan_VPTree.exe Spiral.csv n n %%A %%E %%N 0.5 0.5 Spiral_Out3_%%A_%%E_%%N.txt
FOR /L %%A IN (2,1,2) DO FOR /L %%E IN (3,1,3) DO FOR /L %%N IN (3,1,3) DO .\DBScan_VPTree.exe Spiral.csv n n %%A %%E %%N 0.9 0.9 Spiral_Out3_%%A_%%E_%%N.txt