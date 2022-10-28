#include <ctype.h>
#include <string.h>
/* trim: elimina blancos, tabuladores y nueva línea al final */

/* Kernighan, B. D. y D. M. Ritchie (1991).  El lenguaje de programacion C,
	Prentice-Hall Hispanoamericana, Segunda Edicion, p. 71  */

int trim(char s[])
{
	int n;

	for (n = strlen(s)-1; n >= 0; n--)
		if (!isspace(s[n]))
			break;
	s[n+1] = '\0';
	return n;
}
