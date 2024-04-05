#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "lexer.h"
#include "ad.h"
#include "utils.h"

Token *iTk;//the iterator in the tokens list
Token *consumedTk;//the last consumed token

Symbol* owner = NULL;

//declararea functiilor
bool unit();
bool structDef();
bool varDef();
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
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
    //printf("#unit %d\n",iTk->line);
	for(;;){ //for infinit, pentru a consuma oricate definitii de structuri, functii, variabile
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break;//daca niciuna din definitii nu se consuma!
    }
	if(consume(END)){//terminatorul de fisier
		return true;//gata 
    }else tkerr("syntax error");
	return false;
}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON
bool structDef(){
    //printf("#structDef %d\n",iTk->line);
    Token *start = iTk;
    
    if(consume(STRUCT)){
        if(consume(ID)){
            Token *tkName = consumedTk;
            if(consume(LACC)){
                Symbol *s=findSymbolInDomain(symTable,tkName->text);
                if(s)tkerr("symbol redefinition: %s",tkName->text);
                s=addSymbolToDomain(symTable,newSymbol(tkName->text,SK_STRUCT));
                s->type.tb=TB_STRUCT;
                s->type.s=s;
                s->type.n=-1;
                pushDomain();
                owner=s;
                for(;;){
                    if(varDef()){}
                    else break;
                }
                if(consume(RACC)){
                    if(consume(SEMICOLON)){
                        owner=NULL;
                        dropDomain();
                        return true;
                    }else tkerr("lipseste ; dupa definirea structurii");
                }else tkerr("lipseste } la finalul structurii");
            }
        }else tkerr("lipseste numele structurii");
    }
    
    iTk = start;
    return false;

}

// varDef: typeBase ID arrayDecl? SEMICOLON
bool varDef(){
    //printf("#varDef %d\n",iTk->line);
    Token *start = iTk;
    Type t; 

    if(typeBase(&t)){
        if(consume(ID)){
            Token *tkName = consumedTk;
            if(arrayDecl(&t)){
                if(t.n==0)tkerr("a vector variable must have a specified dimension");
            }
            if(consume(SEMICOLON)){
                Symbol *var=findSymbolInDomain(symTable,tkName->text);
                if(var)tkerr("symbol redefinition: %s",tkName->text);
                var=newSymbol(tkName->text,SK_VAR);
                var->type=t;
                var->owner=owner;
                addSymbolToDomain(symTable,var);
                if(owner){
                    switch(owner->kind){
                    case SK_FN:
                        var->varIdx=symbolsLen(owner->fn.locals);
                        addSymbolToList(&owner->fn.locals,dupSymbol(var));
                        break;
                    case SK_STRUCT:
                        var->varIdx=typeSize(&owner->type);
                        addSymbolToList(&owner->structMembers,dupSymbol(var));
                        break;
                    default : break;
                    }
                }else{
                    var->varMem=safeAlloc(typeSize(&t));
                }
                return true;
            }else tkerr("lipseste ; dupa definirea variabilei");
        }else tkerr("lipseste numele variabilei");
    }
    iTk = start;
    return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t){
    //printf("#typeBase %d\n",iTk->line);
    t->n=-1;
    Token *start = iTk;//tinem minte iteratorul la inceputul fiecarei reguli
	if(consume(TYPE_INT)){//daca la pozitia curenta avem "int", il consuma si trece mai departe
        t->tb=TB_INT;
		return true;
    }
	if(consume(TYPE_DOUBLE)){//daca la pozitia curenta avem "double", il consuma si trece mai departe
        t->tb=TB_DOUBLE;
		return true;
    }
	if(consume(TYPE_CHAR)){//daca la pozitia curenta avem "char", il consuma si trece mai departe
        t->tb=TB_CHAR;
		return true;
    }
	if(consume(STRUCT)){//daca la pozitia curenta avem "struct", il consuma si trece mai departe
		if(consume(ID)){//id-ul de la struct
            Token *tkName = consumedTk;
            t->tb=TB_STRUCT;
            t->s=findSymbol(tkName->text);
            if(!t->s) tkerr("structura nedefinita: %s",tkName->text);
			return true;
        }else tkerr("lipseste numele structurii");
    }
    iTk = start;//refacere pozitie initiala
	return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(Type *t){
    if(consume(LBRACKET)){
        if(consume(INT)){
            Token *tkSize=consumedTk;
            t->n=tkSize->i;
        }else{
            t->n=0; // array fara dimensiune: int v[]
        }
        if(consume(RBRACKET)){
            return true;
        }else tkerr("missing ] or invalid expression inside [...]");
    }
    return false;
}

/*
fnDef: ( typeBase | VOID ) ID
LPAR ( fnParam ( COMMA fnParam )* )? RPAR
stmCompound
*/
bool fnDef(){
    //printf("#fnDef %d\n",iTk->line);
    Token *start = iTk;
    Type t;

    if(typeBase(&t)){
        if(consume(ID)){
            Token *tkName = consumedTk;
            if(consume(LPAR)){
                Symbol *fn=findSymbolInDomain(symTable,tkName->text);
                if(fn)tkerr("symbol redefinition: %s",tkName->text);
                fn=newSymbol(tkName->text,SK_FN);
                fn->type=t;
                addSymbolToDomain(symTable,fn);
                owner=fn;
                pushDomain();
                if(fnParam()){
                    for(;;){
                        if(consume(COMMA)){
                            if(fnParam()){}
                            else tkerr("lipseste parametrul functiei dupa ,");
                        }else break;
                    }
                }
                if(consume(RPAR)){
                    if(stmCompound(false)){
                        dropDomain();
                        owner=NULL;
                        return true;
                    }tkerr("lipseste corpul functiei");
                }else tkerr("lipseste ) la finalul functiei");
            }
        }else tkerr("lipseste numele functiei");
    }else if(consume(VOID)){
        t.tb=TB_VOID;
        if(consume(ID)){
            Token *tkName = consumedTk;
            if(consume(LPAR)){
                Symbol *fn=findSymbolInDomain(symTable,tkName->text);
                if(fn)tkerr("symbol redefinition: %s",tkName->text);
                fn=newSymbol(tkName->text,SK_FN);
                fn->type=t;
                addSymbolToDomain(symTable,fn);
                owner=fn;
                pushDomain();
                if(fnParam()){
                    for(;;){
                        if(consume(COMMA)){
                            if(fnParam()){}
                            else tkerr("lipseste parametrul functiei dupa ,");
                        }else break;
                    }
                }
                if(consume(RPAR)){
                    if(stmCompound(false)){
                        dropDomain();
                        owner=NULL;
                        return true;
                    }tkerr("lipseste corpul functiei");
                }else tkerr("lipseste ) la finalul functiei");
            }
        }else tkerr("lipseste numele functiei");
    }
    iTk = start;
    return false;
}

// fnParam: typeBase ID arrayDecl?
bool fnParam(){
    //printf("#fnParam %d\n",iTk->line);
    Token *start = iTk;
    Type t;

    if(typeBase(&t)){
        if(consume(ID)){
            Token *tkName = consumedTk;
            if(arrayDecl(&t)){
                t.n=0;
            }
            Symbol *param=findSymbolInDomain(symTable,tkName->text);
            if(param)tkerr("symbol redefinition: %s",tkName->text);
            param=newSymbol(tkName->text,SK_PARAM);
            param->type=t;
            param->owner=owner;
            param->paramIdx=symbolsLen(owner->fn.params);
            // parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
            addSymbolToDomain(symTable,param);
            addSymbolToList(&owner->fn.params,dupSymbol(param));
            return true;
        }else tkerr("lipseste numele parametrului functiei");
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
    //printf("#stm %d\n",iTk->line);
    Token *start = iTk;

    if(stmCompound(true)){
        return true;
    }
    if(consume(IF)){
        if(consume(LPAR)){
            if(expr()){
                if(consume(RPAR)){
                    if(stm()){
                        if(consume(ELSE)){
                            if(stm()){
                                return true;
                            }
                            return false;
                            iTk = start;
                        }
                        return true;
                    }
                }else tkerr("lipseste ) dupa conditia if-ului");
            }else tkerr("conditie if invalida");
        }else tkerr("lipseste ( dupa if");
    }
    if(consume(WHILE)){
        if(consume(LPAR)){
            if(expr()){
                if(consume(RPAR)){
                    if(stm()){
                        return true;
                    }
                }else tkerr("lipseste ) dupa conditia while-ului");
            }else tkerr("conditie while invalida");
        }else tkerr("lipseste ( dupa while");
        iTk = start;
    }
    if(consume(RETURN)){
        if(expr()){}
        if(consume(SEMICOLON)){
            return true;
        }else tkerr("lipseste ; dupa return");
        iTk = start;
    }
  
    if(expr()){
        if(consume(SEMICOLON)){
            return true;
        }else{
            tkerr("lipseste ;");
        }
    }
    if(consume(SEMICOLON)){
        return true;
    }       
    iTk = start;
    return false;
}

// stmCompound: LACC ( varDef | stm )* RACC
bool stmCompound(bool newDomain){
    //printf("#stmCompound %d\n",iTk->line);
    Token *start = iTk;

    if(consume(LACC)){
        if(newDomain)pushDomain();
        for(;;){
            if(varDef() || stm()){}
            else break;
        }
        if(consume(RACC)){
            if(newDomain)dropDomain();
            return true;
        }else tkerr("lipseste } la inchiderea blocului");

    }
    iTk = start;
    return false;
}

//expr: exprAssign
bool expr(){
    //printf("#expr %d\n",iTk->line);
    Token *start = iTk;

    if(exprAssign()){
        return true;
    }
    iTk = start;
    return false;
}

//exprAssign: exprUnary ASSIGN exprAssign | exprOr
bool exprAssign(){
    //printf("#exprAssign %d\n",iTk->line);
    Token *start = iTk;

    if(exprUnary()){
        if(consume(ASSIGN)){
            if(exprAssign()){
                return true;
            }else tkerr("lipseste expresia dupa semnul =");
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
    //printf("#exprOrPrim %d\n",iTk->line);
    if(consume(OR)){
        if(exprAnd()){
            if(exprOrPrim()){
                return true;
            }
        }else tkerr("lipseste expresia de dupa ||");
    }
    return true;//epsilon
}

bool exprOr(){
    //printf("#exprOr %d\n",iTk->line);
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
    //printf("#exprAndPrim %d\n",iTk->line);
    if(consume(AND)){
        if(exprEq()){
            if(exprAndPrim()){
                return true;
            }
        }else tkerr("lipseste expresia de dupa &&");
    }
    return true; //epsilon
}

bool exprAnd(){
    //printf("#exprAnd %d\n",iTk->line);
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
    //printf("#exprEqPrim %d\n",iTk->line);
    if(consume(EQUAL) || consume(NOTEQ)){
        if(exprRel()){
            if(exprEqPrim()){
                return true;
            }
        }else tkerr("lipseste expresia de dupa == sau !=");
    }
    return true;//epsilon
}

bool exprEq(){
    //printf("#exprEq %d\n",iTk->line);
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
    //printf("#exprRelPrim %d\n",iTk->line);
    if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)){
        if(exprAdd()){
            if(exprRelPrim()){
                return true;
            }
        }else tkerr("lipseste expresia de dupa < sau <= sau > sau >=");
    }
    return true;//epsilon
}

bool exprRel(){
    //printf("#exprRel %d\n",iTk->line);
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
    //printf("#exprAddPrim %d\n",iTk->line);
    if(consume(ADD) || consume(SUB)){
        if(exprMul()){
            if(exprAddPrim()){
                return true;
            }
        }else tkerr("lipseste expresia de dupa semnul + sau -");
    }
    return true;//epsilon
}

bool exprAdd(){
    //printf("#exprAdd %d\n",iTk->line);
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
    //printf("#exprMulPrim %d\n",iTk->line);
    if(consume(MUL) || consume(DIV)){
        if(exprCast()){
            if(exprMulPrim()){
                return true;
            }
        }else tkerr("lipseste expresia de dupa semnul * sau /");
    }
    return true;//epsilon
}

bool exprMul(){
    //printf("#exprMul %d\n",iTk->line);
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
    //printf("#exprCast %d\n",iTk->line);
    Token *start = iTk;

    if(consume(LPAR)){
        Type t;
        if(typeBase(&t)){
            if(arrayDecl(&t)){}
            if(consume(RPAR)){
                if(exprCast()){
                    return true;
                }else tkerr("lipseste expresia de la Type Cast");
            }else tkerr("lipseste ) la Type Casting");
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
    //printf("#exprUnary %d\n",iTk->line);
    Token *start = iTk;

    if(consume(SUB) || consume(NOT)){
        if(exprUnary()){
            return true;
        }else tkerr("expresie invalida dupa - sau !");
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
    //printf("#exprPostfixPrim %d\n",iTk->line);
    if(consume(LBRACKET)){
        if(expr()){
            if(consume(RBRACKET)){
                if(exprPostfixPrim()){
                    return true;
                }
            }else tkerr("lipseste ]");
        }
        iTk = start;
    }
    if(consume(DOT)){
        if(consume(ID)){
            if(exprPostfixPrim()){
                return true;
            }
        }else tkerr("lipseste numele campului ce se doreste a fi accesat");
        iTk = start;
    }
    return true;//epsilon

}

bool exprPostfix(){
    //printf("#exprPostfix %d\n",iTk->line);
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
    //printf("#exprPrimary  %d\n",iTk->line);
    Token *start = iTk;

    if(consume(ID)){
        if(consume(LPAR)){
            if(expr()){
                for(;;){
                    if(consume(COMMA)){
                        if(expr()){}
                        else tkerr("lipseste expresie dupa ,");
                    }else break;
                }
            }
            if(consume(RPAR)){}
            else tkerr("lipseste ) in apelul functiei");
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
            }else tkerr("lipseste ) la finalul expresiei");
        }else tkerr("lipseste expresia");
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