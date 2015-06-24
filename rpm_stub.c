#include <stdio.h>
#include <string.h>
#include <rpm/rpmts.h>
#include <rpm/rpmfi.h>
#include <rpm/rpmlib.h>

typedef struct string_list_ {
  size_t count;
  char ** strings;
} string_list;

string_list *
string_list_new(void)
{
  return calloc(1, sizeof(string_list));
}

void
string_list_free(string_list * sl)
{
  for (int i = 0; i < sl->count; ++i) {
    free(sl->strings[i]);
  }
  free(sl->strings);
  free(sl);
}

void
string_list_append(string_list * sl, const char * string)
{
  ++sl->count;
  sl->strings = realloc(sl->strings, sl->count * sizeof(char *));
  sl->strings[sl->count - 1] = strdup(string);
}

string_list *
rpm_file_list(const char * pkg)
{
  rpmReadConfigFiles(NULL, NULL);
  FD_t fd = Fopen(pkg, "r.ufdio");

  rpmts ts = rpmtsCreate();
  Header h;
  rpmReadPackageFile(ts, fd, NULL, &h);
  rpmtsFree(ts);

  string_list * sl = string_list_new();
  rpmfi fi = rpmfiNew(NULL, h, RPMTAG_BASENAMES, RPMFI_KEEPHEADER);

  if (fi) {
    while (rpmfiNext(fi) != -1) {
      string_list_append(sl, rpmfiFN(fi));
    }
    fi = rpmfiFree(fi);
  }

  return sl;
}
