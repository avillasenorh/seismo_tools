/*******************************************************************************
* Extracts m characters of sting t starting at character number n and stores
* output in string s
*
* Returns the total number of characters copied to s (can be smaller than m)
*
* Adds terminating null character to s
*
* s must be at least m+1 characters long. If not, behaviour is undefined
*******************************************************************************/

int substr(char *s, char *t, int n, int m)
{
	int	i, j;

	i = 0;
	j = n-1;
	while (i < m && (s[i++] = t[j++]) != '\0')
		;
	s[i] = '\0';

	return i;
}
