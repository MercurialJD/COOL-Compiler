# Path relative to the src directory
# TEST_FILE="../../TEST/finally_exception.cl"
TEST_FILE="../../TEST/longcomment.cool"
MY_OUT="./my_out"
REF_OUT="./ref_out"


# Build and run
make
../../reference-binaries/lexer $TEST_FILE | ./parser &> $MY_OUT
../../reference-binaries/lexer $TEST_FILE | ../../reference-binaries/parser &> $REF_OUT
# ../../reference-binaries/lexer $TEST_FILE | ./parser -lvps &> $MY_OUT
# ../../reference-binaries/lexer $TEST_FILE | ../../reference-binaries/parser -lvps &> $REF_OUT


# Comapre
echo; echo "<-- Begin comparison -->"; echo;
# diff -y --width=155 $MY_OUT $REF_OUT
# diff -y --suppress-common-lines --width=155 $MY_OUT $REF_OUT
icdiff $MY_OUT $REF_OUT
echo; echo "<-- End comparison -->"; echo;


# Clean temp files
# rm $MY_OUT $REF_OUT
# make clean &> /dev/null
