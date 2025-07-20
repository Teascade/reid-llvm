struct DivT {
  int quot;
  int rem;
};

extern struct DivT div(int num, int denom);

int main() { return div(10, 5).quot; }