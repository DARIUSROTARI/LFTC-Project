#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;	// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){

	Token *tk=safeAlloc(sizeof(Token));//se aloca spatiu pentru noul token
	tk->code=code;//se adauga code-ul 
	tk->line=line;//se adauga linia la care este intalnit 
	tk->next=NULL;//deocamdata, este ultimul token din lista 

	if(lastTk){
		lastTk->next=tk;//daca nu e primul
	}else{
		tokens=tk;//daca e primul
	}

	lastTk=tk;//ultimul token din lista este chiar token-ul nou adaugat
	return tk;//returneaza un pointer la atomul adaugat

}

char *extract(const char *begin,const char *end){

	if (end < begin) {
    	err("Not a valid string segment");
  	}

  	size_t size_of_segment = end - begin;//dimensiunea segmentului

  	char *segment = (char *)safeAlloc(size_of_segment * sizeof(char) + 1);//alocarea memoriei + 1 pentru terminatorul de sir \0
 	strncpy(segment, begin, size_of_segment);//copiaza in segment size_of_segment caractere incepand de la begin

  	segment[size_of_segment] = '\0';//pozitionarea terminatorului de sir

  	return segment;//returneaza segmentul extras
}

//primeste ca parametru un pointer la fisierul de intrare
Token *tokenize(const char *pch){

	const char *start;
	Token *tk;

	for(;;){//for infinit
		switch(*pch){//switch cu caracterul nostru curent
			case ' ':case '\t':pch++;break;//spatiu sau tab => trecem mai departe, doar le consumam, nu facem nimic cu ele
			case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
				if(pch[1]=='\n')pch++;
				// fallthrough to \n
			case '\n':
				line++;//creste line, deoarece trecem la o linie noua
				pch++;//trecem la urmatorul caracter
				break;
			case '\0':addTk(END);return tokens;//sfarsit de fisier, se adauga END si returnam lista de atomi si se iese din bucla infinita si se iese din functie
			case ',':addTk(COMMA);pch++;break;
			case '=':
				if(pch[1]=='='){//verificam daca si urmatorul caracter este tot '='
					addTk(EQUAL);//adaugam atomul in lista de atomi
					pch+=2;//trecem peste ambii "=="
				}else{
					addTk(ASSIGN);//adaugam atomul ASSIGN in lista de atomi
					pch++;//trecem la urmatorul caracter
				}
				break;
			case ';':addTk(SEMICOLON);pch++;break;
			case '(':addTk(LPAR);pch++;break;
			case ')':addTk(RPAR);pch++;break;
			case '[':addTk(LBRACKET);pch++;break;
			case ']':addTk(RBRACKET);pch++;break;
			case '{':addTk(LACC);pch++;break;
			case '}':addTk(RACC);pch++;break;
			case '+':addTk(ADD);pch++;break;
			case '-':addTk(SUB);pch++;break;
			case '*':addTk(MUL);pch++;break;
			case '/':
				if(pch[1] == '/'){// comentariu de tip //...
					//comentariul nu reprezinta un atom lexical
					while(*pch && *pch!= '\n') pch++; //ignora restul liniei
				}else if(pch[1] == '*'){// comentariul de tipul /* ... */
					pch+=2;//trecem peste '*'
					while(*pch && !(*pch == '*' && pch[1] == '/')) pch++; //ignora comentariul pana la sfarsitul acestuia marcat de */
					pch+=2; //trecem peste */
				}else{
					addTk(DIV);
					pch++;
				}
				break;
			case '.':addTk(DOT);pch++;break;
			case '&':
				if(pch[1] == '&'){
					addTk(AND);
					pch+=2;
				}else{
					err("invalid character");
				}
				break;
			case '|':
				if(pch[1] == '|'){
					addTk(OR);
					pch+=2;
				}else{
					err("invalid character");
				}
				break;
			case '!':
				if(pch[1] == '='){
					addTk(NOTEQ);
					pch+=2;
				}else{
					addTk(NOT);
					pch++;
				}
				break;
			case '<':
				if(pch[1] == '='){
					addTk(LESSEQ);
					pch+=2;
				}else{
					addTk(LESS);
					pch++;
				}
				break;
			case '>':
				if(pch[1] == '='){
					addTk(GREATEREQ);
					pch+=2;
				}else{
					addTk(GREATER);
					pch++;
				}
				break;
			case '"':
				for(start = ++pch ; *pch && *pch != '"' ; ++pch){}
				if(*pch == '"'){
					addTk(STRING)->text = extract(start,pch);
					++pch;//trecem peste a 2-a ghilimea 
				}else{
					err("String neinchis !");
				}
				break;
			case '\'':
				if(pch[2] == '\''){
					addTk(CHAR)->c = pch[1];
					pch+=3;
				}
				else{
					err("invalid character !");
				}
				break;
			default://pentru clase de caractere(testam cazurile in care un token incepe cu clase de caractere)
				if(isalpha(*pch)||*pch=='_'){
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}//start contine PRIMUL CARACTER, ca este post-incrementare!!!
					//doar parcurgem in for, nu facem nimic
					char *text=extract(start,pch);//pch, dupa for, va fi pe pozitia de DUPA ultimul caracter !! [start,pch)
                    //extract - primeste ca param 2 pointeri si extrage intr-o zona noua de memorie 
                    //DUPA ce am extras subsirul
					if(strcmp(text,"char") == 0)//testam daca ID-ul nu este cumva un cuvant cheie(daca strcmp returneaza 0, inseamna ca am dat un cuvant cheie)
						addTk(TYPE_CHAR);
					else if(strcmp(text,"int") == 0)
							addTk(TYPE_INT);
						 else if(strcmp(text,"double") == 0)
						 		addTk(TYPE_DOUBLE);
							  else if(strcmp(text,"return") == 0)
						      		addTk(RETURN);
								   else if(strcmp(text,"if") == 0)
								   		  addTk(IF);
										else if(strcmp(text,"else") == 0)
											   addTk(ELSE);
											else if(strcmp(text,"struct") == 0)
												   addTk(STRUCT);
												 else if(strcmp(text,"void") == 0)
												 		addTk(VOID);
													  else if(strcmp(text,"while") == 0)
													  		addTk(WHILE);
															else if(strcmp(text,"for") == 0)
																addTk(FOR);											 
										    			   		else{//pe acest else intram cand este ID simplu
					 												tk=addTk(ID);//il adaugam in lista de tokeni
					 												tk->text=text;//la acel atom ii atribuim zona de memorie alocata dinamic
																}
					
                }
				else if (isdigit(*pch) || *pch == '.' || *pch == 'e' || *pch == 'E') {
					int dot = 0;
					int has_exponent = 0;
					char *endptr;

					for (start = pch; isdigit(*pch) || *pch == '.' || *pch == 'e' || *pch == 'E'; pch++) {
						if (*pch == '.') {
							if (dot == 0) {
								dot = 1;
							} else {
								err("invalid number");
							}
						} else if (*pch == 'e' || *pch == 'E') {
							has_exponent = 1;
							break; // Ieșim din buclă la întâlnirea exponențialului
						}
					}

					char *numar = extract(start, pch);

					if (has_exponent == 1) {//daca are exponent, adica daca am intalnit un 'E' sau 'e'

						if(*pch == 'e'){//daca am intalnit un 'e', atunci poate va urma un '-' sau nu
							if(pch[1] == '-')
								pch+=2;//daca urmeaza un '-' dupa caracterul 'e', trecem peste el
							else
								pch++;
							for(start = pch++; isdigit(*pch);pch++){}//iteram numerele ramase sa vedem exponentul lui 10

							char *putere = extract(start, pch);

							double value = strtod(numar, &endptr);
							double exponent = strtod(putere,&endptr);

							if (endptr != numar) {
								exponent = exponent * (-1);//inmultim exponentul cu -1 pentru a avea o putere negativa pentru 10
								addTk(DOUBLE)->d = value * pow(10,exponent);
							} else {
								err("invalid number !");
							}
						}else if(*pch == 'E'){//daca am intalnit un 'E' , atunci poate va urma un '+' sau nu
							if(pch[1] == '+')
								pch+=2;//daca urmeaza un '+' dupa caracterul 'E', trecem peste el
							else
								pch++;
							for(start = pch++; isdigit(*pch);pch++){}//iteram numerele ramase sa vedem exponentul lui 10

							char *putere = extract(start, pch);

							double value = strtod(numar, &endptr);
							double exponent = strtod(putere,&endptr);

							if (endptr != numar) {
								addTk(DOUBLE)->d = value * pow(10,exponent);
							} else {
								err("invalid number !");
							}
						} 
					}else if (dot == 1){ //daca am  intalnit un punct 
						double value = strtod(numar, &endptr);

						if (endptr != numar) {
							addTk(DOUBLE)->d = value;
						} else {
							err("invalid number !");
						}
					}else {// daca nu am intalnit nici punct, nici 'e' sau 'E', inseamna ca avem un numar intreg
						long value = strtol(numar, &endptr, 10);

						if (endptr != numar) {
							addTk(INT)->i = value;
						} else {
							err("invalid number !");
						}
					}

				}else err("invalid char: %c (%d)",*pch,*pch);//se ajunge aici cand in switch nu s-a mers pe niciun case si nu s-a mers nici pe if-ul din default
		}
	}
}

void showTokens(const Token *tokens){

	for(const Token *tk=tokens;tk;tk=tk->next){
		
		printf("%d ",tk->line);//AFISEAZA IN FORMAT NUMERIC CODUL FIECARUI ATOM, TREBUIE RECOMPLETATA!

		switch(tk->code){
			case ID: printf("ID:%s\n",tk->text);break;
			case TYPE_CHAR: printf("TYPE_CHAR\n");break;
			case TYPE_DOUBLE: printf("TYPE_DOUBLE\n");break;
			case ELSE: printf("ELSE\n");break;
			case IF: printf("IF\n");break;
			case TYPE_INT: printf("TYPE_INT\n");break;
			case RETURN: printf("RETURN\n");break;
			case STRUCT: printf("STRUCT\n");break;
			case VOID: printf("VOID\n");break;
			case WHILE: printf("WHILE\n");break;
			case FOR: printf("FOR\n");break;
			case INT: printf("INT:%d\n",tk->i);break;
			case DOUBLE: printf("DOUBLE:%g\n",tk->d);break;
			case CHAR: printf("CHAR:%c\n",tk->c);break;
			case STRING: printf("STRING:%s\n",tk->text);break;
			case COMMA: printf("COMMA\n");break;
			case SEMICOLON: printf("SEMICOLON\n");break;
			case LPAR: printf("LPAR\n");break;
			case RPAR: printf("RPAR\n");break;
			case LBRACKET: printf("LBRACKET\n");break;
			case RBRACKET: printf("RBRACKET\n");break;
			case LACC: printf("LACC\n");break;
			case RACC: printf("RACC\n");break;
			case END: printf("END\n");break;
			case ADD: printf("ADD\n");break;
			case SUB: printf("SUB\n");break;
			case MUL: printf("MUL\n");break;
			case DIV: printf("DIV\n");break;
			case DOT: printf("DOT\n");break;
			case AND: printf("AND\n");break;
			case OR: printf("OR\n");break;
			case NOT: printf("NOT\n");break;
			case ASSIGN: printf("ASSIGN\n");break;
			case EQUAL: printf("EQUAL\n");break;
			case NOTEQ: printf("NOTEQ\n");break;
			case LESS: printf("LESS\n");break;
			case LESSEQ: printf("LESSEQ\n");break;
			case GREATER: printf("GREATER\n");break;
			case GREATEREQ: printf("GREATEREQ\n");break;
			default: err("Token necunoscut !");
		}
	}
}