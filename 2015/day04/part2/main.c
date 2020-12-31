#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <openssl/md5.h>

int main(int argc, char **argv)
{
  if (argc != 2)
  {
    fprintf(stderr, "Usage: ./main.o <input>");
    return 1;
  }

  MD5_CTX md5handler;
  unsigned char md5digest[MD5_DIGEST_LENGTH];
  char *guess;
  char *str_num;
  char *input_string = argv[1];
  int input_len = strlen(input_string);

  MD5_Init(&md5handler);

  for (unsigned int i = 0; i < (int)pow(10, input_len - 1); i++)
  {
    asprintf(&str_num, "%d", i);
    asprintf(&guess, "%s%s", input_string, str_num);

    MD5((unsigned char *)guess, strlen(guess), md5digest);

    // I'm 100% sure that there is better approach, but I'm too lazy to google it.
    if (md5digest[0] == 0 && md5digest[1] == 0 && md5digest[2] == 0)
    {
      printf("%s\n", str_num);
      return 0;
    }
  }

  fprintf(stderr, "Cannot find any solution");

  return 1;
}