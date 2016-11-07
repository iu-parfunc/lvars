set terminal postscript eps enhanced color font 'Helvetica,20'
set output "fold.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Threads"
set xtics 1
set ylabel "Parallel speedup (relative to ParFoldable)"
set key bottom right
set datafile separator ","

pmapfold_init=system("head -1 reports/pmapfold.csv | cut -f2 -d,")

plot "reports/pmapfold.csv" using 1:((pmapfold_init/$2)) \
     with errorlines linewidth 1.5 pointtype 4 pointsize 1.0 \
     title "ParFoldable", \
     "reports/vpmapfold.csv" using 1:((pmapfold_init/$2)) \
     with errorlines linewidth 1.5 pointtype 7 pointsize 1.0 \
     title "Verified ParFoldable"
