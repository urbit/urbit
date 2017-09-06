#include <iostream>
#include <thread>

/** Print number of (enabled) CPU cores.
 *
 * Requires C++11 or better.
 */
int main() {
  unsigned int numcpus = std::thread::hardware_concurrency();
  std::cout << (numcpus > 0 ? numcpus : 1) << std::endl;
}
