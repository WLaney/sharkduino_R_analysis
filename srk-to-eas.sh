cd /Users/Centigonal/Sharkduino/Arduino_Animal_Tag/binary/parser/
echo "$1"
./shark-parser "$1" tmp-data.csv tmp-header.csv
mv tmp-data.csv /Users/Centigonal/Sharkduino/sharkduino_R_analysis/data
cd /Users/Centigonal/Sharkduino/sharkduino_R_analysis
Rscript extractEAs.R




