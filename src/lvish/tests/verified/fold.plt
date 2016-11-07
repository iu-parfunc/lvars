set terminal postscript eps enhanced color font 'Helvetica,20'
set output "fold.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Threads"
set xtics 1
set ylabel "Time in seconds"
set datafile separator ","
plot "reports/pmapfold.csv" using 1:2 \
     with errorlines linewidth 1.5 pointtype 4 pointsize 1.0 \
     title "ParFoldable", \
     "reports/vpmapfold.csv" using 1:2 \
     with errorlines linewidth 1.5 pointtype 7 pointsize 1.0 \
     title "Verified ParFoldable"
