#include <stdio.h>
#include "rpm_stub.c"

int
main(int argc, char ** argv)
{
  string_list * sl = rpm_file_list(argv[1]);
  for (int i = 0; i < sl->count; ++i) {
    fprintf(stdout, "%s\n", sl->strings[i]);
  }
  string_list_free(sl);
  return 0;
}
