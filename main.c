#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"
#include "parser.h"

int main(){

    char *inbuf = loadFile("tests2/testparser.c");//se incarca continutul fisierului
    
    Token *tokens = tokenize(inbuf);//returneaza lista de token-uri

    showTokens(tokens);//afiseaza lista de token-uri

    free(inbuf);//eliberam memoria alocata

    parse(tokens);//parsam lista de tokeni
    
    return 0;
}