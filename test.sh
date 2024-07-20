# Path relative to the src directory
TEST_FILE="../../TEST/longstring_escapedbackslashes.cool"
MY_OUT="./my_out"
REF_OUT="./ref_out"

# Build and run
make
./lexer $TEST_FILE > $MY_OUT && ../../reference-binaries/lexer $TEST_FILE > $REF_OUT

# Comapre
echo; echo "<-- Begin comparison -->"; echo;
diff -y --suppress-common-lines $MY_OUT $REF_OUT
echo; echo "<-- End comparison -->"; echo;

# Clean temp files
rm $MY_OUT $REF_OUT
# make clean &> /dev/null
