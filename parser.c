#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "lexer.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

//declararea functiilor
bool unit();
bool structDef();
bool varDef();
bool typeBase();
bool arrayDecl();
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound();
bool expr();
bool exprAssign();
bool exprOr();
bool exprOrPrim();
bool exprAnd();
bool exprAndPrim();
bool exprEq();
bool exprEqPrim();
bool exprRel();
bool exprRelPrim();
bool exprAdd();
bool exprAddPrim();
bool exprMul();
bool exprMulPrim();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPostfixPrim();
bool exprPrimary();

void tkerr(const char *fmt,...){// functia pentru emiterea erorilor 
	fprintf(stderr,"error in line %d: ",iTk->line);//afiseaza la ce linie este eroarea
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);//afiseaza textul erorii
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);//iese din program
}

bool consume(int code){
	if(iTk->code==code){//daca la pozitia curenta a iteratorului avem un token a carui cod este transmis ca si parametru
		consumedTk=iTk;//il consuma
		iTk=iTk->next;//trecem mai departe
		return true;
	}
	return false;//daca nu, ramanem pe loc
}

// unit: ( structDef | fnDef | varDef )* END
bool unit(){ // fiecare regula e implementata printr-o functie a ei
    printf("#unit %d\n",iTk->line);
	for(;;){ //for infinit, pentru a consuma oricate definitii de structuri, functii, variabile
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break;//daca niciuna din definitii nu se consuma!
    }
	if(consume(END)){//terminatorul de fisier
		return true;//gata 
    }
	return false;
}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef(){
    printf("#structDef %d\n",iTk->line);
    Token *start = iTk;
    
    if(consume(STRUCT)){
        if(consume(ID)){
            if(consume(LACC)){
                for(;;){
                    if(varDef()){}
                    else break;
                }
                if(consume(RACC)){
                    if(consume(SEMICOLON)){
                        return true;
                    }
                }
            }
        }
    }
    
    iTk = start;
    return false;

}

// varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef(){
    printf("#varDef %d\n",iTk->line);
    Token *start = iTk;

    if(typeBase()){
        if(consume(ID)){
            if(arrayDecl()){}
            if(consume(SEMICOLON)){
                return true;
            }
        }
    }
    iTk = start;
    return false;

}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(){//atomi lexicali - folosim consume()
    printf("#typeBase %d\n",iTk->line);
    Token *start = iTk;//tinem minte iteratorul la inceputul fiecarei reguli
	if(consume(TYPE_INT)){//daca la pozitia curenta avem "int", il consuma si trece mai departe
		return true;
    }
	if(consume(TYPE_DOUBLE)){//daca la pozitia curenta avem "double", il consuma si trece mai departe
		return true;
    }
	if(consume(TYPE_CHAR)){//daca la pozitia curenta avem "char", il consuma si trece mai departe
		return true;
    }
	if(consume(STRUCT)){//daca la pozitia curenta avem "struct", il consuma si trece mai departe
		if(consume(ID)){//id-ul de la struct
			return true;
        }
    }
    iTk = start;//refacere pozitie initiala
	return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(){
    printf("#arrayDecl %d\n",iTk->line);
    Token *start = iTk;

    if(consume(LBRACKET)){
        if(consume(INT)){} 
        if(consume(RBRACKET)){
            return true;
        }
    }
    iTk = start;
    return false;
}

/*
fnDef: ( typeBase | VOID ) ID
LPAR ( fnParam ( COMMA fnParam )* )? RPAR
stmCompound
*/
bool fnDef(){
    printf("#fnDef %d\n",iTk->line);
    Token *start = iTk;

    if(typeBase() || consume(VOID)){
        if(consume(ID)){
            if(consume(LPAR)){
                if(fnParam()){
                    for(;;){
                        if(consume(COMMA)){
                            if(fnParam()){}
                        }else break;
                    }
                }
                if(consume(RPAR)){
                    if(stmCompound()){
                        return true;
                    }
                }
            }
        }

    }
    iTk = start;
    return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam(){
    printf("#fnParam %d\n",iTk->line);
    Token *start = iTk;

    if(typeBase()){
        if(consume(ID)){
            if(arrayDecl()){}
            return true;
        }
    }
    iTk = start;
    return false;
}

/*
stm: stmCompound
| IF LPAR expr RPAR stm ( ELSE stm )? 
| WHILE LPAR expr RPAR stm
| RETURN expr? SEMICOLON
| expr? SEMICOLON
*/
bool stm(){
    printf("#stm %d\n",iTk->line);
    Token *start = iTk;

    if(stmCompound()){
        return true;
    }
    if(consume(IF)){
        if(consume(LPAR)){
            if(expr()){
                if(consume(RPAR)){
                    if(stm()){
                        if(consume(ELSE)){
                            if(stm()){}
                            else return false;
                        }
                        return true;
                    }
                }
            }
        }
        iTk = start;
    }
    if(consume(WHILE)){
        if(consume(LPAR)){
            if(expr()){
                if(consume(RPAR)){
                    if(stm()){
                        return true;
                    }
                }
            }
        }
        iTk = start;
    }
    if(consume(RETURN)){
        if(expr()){}
        if(consume(SEMICOLON)){
            return true;
        }
        iTk = start;
    }
    if(expr()){}
    if(consume(SEMICOLON)){
        return true;
    }                
            
    iTk = start;
    return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound(){
    printf("#stmCompound %d\n",iTk->line);
    Token *start = iTk;

    if(consume(LACC)){
        for(;;){
            if(varDef() || stm()){}
            else break;
        }
        if(consume(RACC)){
            return true;
        }
    }
    iTk = start;
    return false;
}

//expr: exprAssign
bool expr(){
    printf("#expr %d\n",iTk->line);
    Token *start = iTk;

    if(exprAssign()){
        return true;
    }
    iTk = start;
    return false;
}

//exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(){
    printf("#exprAssign %d\n",iTk->line);
    Token *start = iTk;

    if(exprUnary()){
        if(consume(ASSIGN)){
            if(exprAssign()){
                return true;
            }
        }
        iTk = start;
    }
    if(exprOr()){
        return true;
    }
    iTk = start;
    return false;
}

/*
exprOr: exprOr OR exprAnd | exprAnd
=> aplicand formula:
A:exprOr
alfa1: OR exprAnd
beta1: exprAnd

exprOr : exprAnd exprOrPrim
exprOrPrim : OR exprAnd exprOrPrim | epsilon
*/
bool exprOrPrim(){ 
    printf("#exprOrPrim %d\n",iTk->line);
    if(consume(OR)){
        if(exprAnd()){
            if(exprOrPrim()){
                return true;
            }
        }
    }
    return true;//epsilon
}

bool exprOr(){
    printf("#exprOr  %d\n",iTk->line);
    Token *start = iTk;

    if(exprAnd()){
        if(exprOrPrim()){
            return true;//am ajuns la capatul regulii
        }
    }
    iTk = start;
    return false;
}

/*
exprAnd: exprAnd AND exprEq | exprEq
=> aplicand formula:
A:exprAnd
alfa1: AND exprEq
beta1: exprEq

exprAnd : exprEq exprAndPrim
exprAndPrim : AND exprEq exprAndPrim | epsilon
*/
bool exprAndPrim(){
    printf("#exprAndPrim %d\n",iTk->line);
    if(consume(AND)){
        if(exprEq()){
            if(exprAndPrim()){
                return true;
            }
        }
    }
    return true; //epsilon
}

bool exprAnd(){
    printf("#exprAnd %d\n",iTk->line);
    Token *start = iTk;

    if(exprEq()){
        if(exprAndPrim()){
            return true;
        }
    }
    iTk = start;
    return false;
}

/*
exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
=> aplicand formula:
A:exprEq
alfa1: ( EQUAL | NOTEQ ) exprRel
beta1: exprRel

exprEq : exprRel exprEqPrim
exprEqPrim : ( EQUAL | NOTEQ ) exprRel exprEqPrim | epsilon
*/
bool exprEqPrim(){
    printf("#exprEqPrim %d\n",iTk->line);
    if(consume(EQUAL) || consume(NOTEQ)){
        if(exprRel()){
            if(exprEqPrim()){
                return true;
            }
        }
    }
    return true;//epsilon
}

bool exprEq(){
    printf("#exprEq %d\n",iTk->line);
    Token *start = iTk;

    if(exprRel()){
        if(exprEqPrim()){
            return true;
        }
    }
    iTk = start;
    return false;
}

/*
exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
=> aplicand formula:
A:exprRel
alfa1: ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd 
beta1: exprAdd

exprRel : exprAdd exprRelPrim
exprRelPrim : ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | epsilon
*/
bool exprRelPrim(){
    printf("#exprRelPrim %d\n",iTk->line);
    if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)){
        if(exprAdd()){
            if(exprRelPrim()){
                return true;
            }
        }
    }
    return true;//epsilon
}

bool exprRel(){
    printf("#exprRel %d\n",iTk->line);
    Token *start = iTk;

    if(exprAdd()){
        if(exprRelPrim()){
            return true;
        }
    }
    iTk = start;
    return false;
}

/*
exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
=> aplicand formula:
A:exprAdd
alfa1: ( ADD | SUB ) exprMul
beta1: exprMul

exprAdd : exprMul exprAddPrim
exprAddPrim : ( ADD | SUB ) exprMul exprAddPrim | epsilon
*/
bool exprAddPrim(){
    printf("#exprAddPrim %d\n",iTk->line);
    if(consume(ADD) || consume(SUB)){
        if(exprMul()){
            if(exprAddPrim()){
                return true;
            }
        }
    }
    return true;//epsilon
}

bool exprAdd(){
    printf("#exprAdd %d\n",iTk->line);
    Token *start = iTk;

    if(exprMul()){
        if(exprAddPrim()){
            return true;
        }
    }
    iTk = start;
    return false;
}

/*
exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
=>folosind formula:
A:exprMul
alfa1: ( MUL | DIV ) exprCast
beta1: exprCast

exprMul : exprCast exprMulPrim
exprMulPrim : ( MUL | DIV ) exprCast exprMulPrim | epsilon
*/
bool exprMulPrim(){
    printf("#exprMulPrim %d\n",iTk->line);
    if(consume(MUL) || consume(DIV)){
        if(exprCast()){
            if(exprMulPrim()){
                return true;
            }
        }
    }
    return true;//epsilon
}

bool exprMul(){
    printf("#exprMul %d\n",iTk->line);
    Token *start = iTk;

    if(exprCast()){
        if(exprMulPrim()){
            return true;
        }
    }
    iTk = start;
    return false;
}

//exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(){
    printf("#exprCast %d\n",iTk->line);
    Token *start = iTk;

    if(consume(LPAR)){
        if(typeBase()){
            if(arrayDecl()){}
            if(consume(RPAR)){
                if(exprCast()){
                    return true;
                }
            }
        }
    }
    if(exprUnary()){
        return true;
    }

    iTk = start;
    return false;
}

//exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
bool exprUnary(){
    printf("#exprUnary %d\n",iTk->line);
    Token *start = iTk;

    if(consume(SUB) || consume(NOT)){
        if(exprUnary()){
            return true;
        }
    }

    if(exprPostfix()){
        return true;
    }
    iTk = start;
    return false;
}

/*
exprPostfix: exprPostfix LBRACKET expr RBRACKET
| exprPostfix DOT ID
| exprPrimary
=> folosind formula:
A: exprPostfix
alfa1: LBRACKET expr RBRACKET
alfa2: DOT ID
beta1:exprPrimary

exprPostfix : exprPrimary exprPostfixPrim
exprPostfixPrim : LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim | epsilon
*/
bool exprPostfixPrim(){
    Token *start = iTk;
    printf("#exprPostfixPrim %d\n",iTk->line);
    if(consume(LBRACKET)){
        if(expr()){
            if(consume(RBRACKET)){
                if(exprPostfixPrim()){
                    return true;
                }
            }
        }
        iTk = start;
    }
    if(consume(DOT)){
        if(consume(ID)){
            if(exprPostfixPrim()){
                return true;
            }
        }
        iTk = start;
    }
    return true;//epsilon

}

bool exprPostfix(){
    printf("#exprPostfix %d\n",iTk->line);
    Token *start = iTk;

    if(exprPrimary()){
        if(exprPostfixPrim()){
            return true;
        }
    }
    iTk = start;
    return false;
}

/*
exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
            | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
*/
bool exprPrimary(){
    printf("#exprPrimary  %d\n",iTk->line);
    Token *start = iTk;

    if(consume(ID)){
        if(consume(LPAR)){
            if(expr()){
                for(;;){
                    if(consume(COMMA)){
                        if(expr()){}
                    }else break;
                }
            }
            if(consume(RPAR)){}
            else return false;
        }
        
        return true;
    }
    if(consume(INT)){
        return true;
    }
    if(consume(DOUBLE)){
        return true;
    }
    if(consume(CHAR)){
        return true;
    }
    if(consume(STRING)){
        return true;
    }
    if(consume(LPAR)){
        if(expr()){
            if(consume(RPAR)){
                return true;
            }
        }
        iTk = start;
    }

    iTk = start;
    return false;
}

void parse(Token *tokens){
	iTk=tokens; //iteratorul in lista de atomi, initializat cu inceputul listei
	if(!unit()) tkerr("syntax error"); //apelam unit
}

//return true :  daca regula a fost indeplinita (daca am ajuns la captul ei, inclusiv END)
//return false : daca regula nu a fost indeplinita