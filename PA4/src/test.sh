# Path relative to the src directory
TEST_FILE="./example.cl"
# TEST_FILE="../../TEST/finally_exception.cl"
# TEST_FILE="../../TEST/longcomment.cool"

MY_ASM_OUT="./my_asm_out.s"
REF_ASM_OUT="./ref_asm_out.s"

MY_SIM_OUT="./my_sim_out.s"
REF_SIM_OUT="./ref_sim_out.s"


# Build and generate assambly
make -j
../tools-bin/lexer $TEST_FILE | ../tools-bin/parser | ../tools-bin/semant | ./cgen > $MY_ASM_OUT
../tools-bin/lexer $TEST_FILE | ../tools-bin/parser | ../tools-bin/semant | ../tools-bin/cgen > $REF_ASM_OUT

# # Comapre assambly
# echo; echo "<-- Begin ASM comparison -->"; echo;
# icdiff $MY_ASM_OUT $REF_ASM_OUT
# echo; echo "<-- End ASM comparison -->"; echo;


# Simulate
spim -exception_file ../tools-bin/trap.s -file $MY_ASM_OUT > $MY_SIM_OUT 2>&1
spim -exception_file ../tools-bin/trap.s -file $REF_ASM_OUT > $REF_SIM_OUT 2>&1

# Comapre simulation
echo; echo "<-- Begin SIM comparison -->"; echo;
icdiff $MY_SIM_OUT $REF_SIM_OUT
echo; echo "<-- End SIM comparison -->"; echo;


# Clean temp files
# rm $MY_ASM_OUT $REF_ASM_OUT
# rm $MY_SIM_OUT $REF_SIM_OUT
make clean &> /dev/null
