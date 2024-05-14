#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "at.h"

int main(){

    char *inbuf = loadFile("tests_at/testat.c");//se incarca continutul fisierului
    
    Token *tokens = tokenize(inbuf);//returneaza lista de token-uri

    //showTokens(tokens);//afiseaza lista de token-uri

    free(inbuf);//eliberam memoria alocata

    pushDomain();

    vmInit();//initializare masina virtuala

    parse(tokens);//parsam lista de tokeni

    //showDomain(symTable,"global");

    Instr *testCode = genTestProgram();//genereaza cod de test pentru masina virtuala

    run(testCode);//executie cod masina virtuala

    dropDomain();
    
    return 0;
}