#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "at.h"

int main(){

    char *inbuf = loadFile("tests_gc/testgc.c");//se incarca continutul fisierului
    
    Token *tokens = tokenize(inbuf);//returneaza lista de token-uri

    //showTokens(tokens);//afiseaza lista de token-uri

    pushDomain();

    vmInit();//initializare masina virtuala

    parse(tokens);//parsam lista de tokeni

//MASINA VIRTUALA:

    Instr *testCode = genTestProgramForMV();//genereaza cod de test pentru masina virtuala

    run(testCode);//executie cod masina virtuala

    printf("\n\n\n");

//GENERAREA DE COD:

    Symbol *symMain=findSymbolInDomain(symTable,"main");

    if(!symMain)err("missing main function");

    Instr *entryCode=NULL;
    addInstr(&entryCode,OP_CALL)->arg.instr=symMain->fn.instr;
    addInstr(&entryCode,OP_HALT);
    run(entryCode);

    //showDomain(symTable,"global");

    dropDomain();
    
    free(inbuf);//eliberam memoria alocata

    return 0;
}