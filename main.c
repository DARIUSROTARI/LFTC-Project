#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"

int main()
{
    char *inbuf = loadFile("tests/testlex.c");//se incarca continutul fisierului
    
    Token *tokens = tokenize(inbuf);//returneaza lista de token-uri

    showTokens(tokens);//afiseaza lista de token-uri

    free(inbuf);//eliberam memoria alocata

    return 0;
}