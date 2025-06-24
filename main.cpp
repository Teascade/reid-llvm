#include <iostream>

extern "C" {
int mainfunc();
}

int main() { std::cout << "Return value of test: " << mainfunc() << std::endl; }
