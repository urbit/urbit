/* include/f/nash.h
**
** This file is in the public domain.
*/
struct u2_nash;

struct u2_nash* u2_na_make();
void u2_na_put(struct u2_nash* nash, u2_noun key, u2_noun val);
u2_weak u2_na_get(struct u2_nash* nash, u2_noun key);
void u2_na_take(struct u2_nash* nash);

