# Generate test data using PBBS.
~/repos/pbbs/testData/graphData/gridGraph -d 3 100000 /tmp/grid

num_cores="1 2 4";

make clean
make BFS_impure
make BFS_pure

# Impure version
for i in $num_cores; do ./BFS_impure -o BFS_impure_$i.html +RTS -N$i; done

# Pure version
for i in $num_cores; do ./BFS_pure -o BFS_pure_$i.html +RTS -N$i; done