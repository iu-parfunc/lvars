set terminal postscript eps enhanced color font 'Helvetica,20'
set output "set.eps"
set format y "%g"
set grid xtics
set grid ytics
set grid ztics
set xlabel "Threads"
set xtics 1
set ylabel "Parallel speedup (relative to PureSet)"
set ytics nomirror
set yrange [0:2]
set y2label "Parallel speedup (relative to SLSet)"
set y2tics nomirror
set y2range [0:8]
set key bottom right
set datafile separator ","

pureset_init=system("head -1 reports/pureset.csv | cut -f2 -d,")
slset_init=system("head -1 reports/slset.csv | cut -f2 -d,")

plot "reports/pureset.csv" using 1:((pureset_init/$2)) \
     with errorlines linewidth 1.5 pointtype 4 pointsize 1.0 \
     title "PureSet" axes x1y1, \
     "reports/vpureset.csv" using 1:((pureset_init/$2)) \
     with errorlines linewidth 1.5 pointtype 7 pointsize 1.0 \
     title "Verified PureSet" axes x1y1, \
     "reports/slset.csv" using 1:((slset_init/$2)) \
     with errorlines linewidth 1.5 pointtype 8 pointsize 1.0 \
     title "SLSet" axes x1y2, \
     "reports/vslset.csv" using 1:((slset_init/$2)) \
     with errorlines linewidth 1.5 pointtype 5 pointsize 1.0 \
     title "Verified SLSet" axes x1y2
