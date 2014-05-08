/* include/f/nash.h
**
** This file is in the public domain.
*/

/** Hash-table design:
***
*** u2_nash is a non-noun hash-table, meant for ephemeral usage such as
*** in jam and cue.
***
*** It uses a Patricia trie keyed on the mug of the actual key, and then
*** a vector of key-value pairs to resolve collisions.
**/

  /* Opaque structure holding hash table.
   */
    struct u2_nash;

  /* Functions
  */

  /* u2_na_make(): create a new nash.
  */
    struct u2_nash*
    u2_na_make();

  /* u2_na_put(): put an entry in the hash table.
  */
    void
    u2_na_put(struct u2_nash* nash, u2_noun key, u2_noun val);

  /* u2_na_get(): retrieve an entry from the hash table, or u2_none.
  */
    u2_weak
    u2_na_get(struct u2_nash* nash, u2_noun key);

  /* u2_na_take(): destroy a nash.
  */
    void
    u2_na_take(struct u2_nash* nash);

